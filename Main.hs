module Main where

import GHC
import DynFlags
import MonadUtils
import Digraph (flattenSCC)
import HscTypes (handleFlagWarnings)
import HscTypes (dep_pkgs)
import PackageConfig
import Packages
import Module
import StaticFlags
 
import GHC.Paths ( libdir )
 
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Set as Set
import System.IO.Error as IOError
import System.IO
import System.Environment.Executable
import Text.PrettyPrint.HughesPJ

main = do
    args <- getArgs
    runGHCWithArgsForTargets args $ do
        modules <- compileAndLoadModules
        targetFiles <- getTargetFiles
        let isTarget m = case ml_hs_file (ms_location m) of
                                Just f | f `elem` targetFiles -> True
                                _ -> False
        let targetModules = filter isTarget modules
        liftIO $ writeFile moduleInitFile $ moduleInitFileContents targetModules
        packageIds <- getUniquePackageIds modules
        maybeLinkFilePath <- liftIO getLinkFilePath
        case maybeLinkFilePath of
            Nothing -> printInstructions args modules packageIds
            Just f -> writeObjectFiles f modules packageIds

printInstructions :: [String] -> [ModSummary] -> [PackageId] -> Ghc ()
printInstructions args modules packageIds = do
    progName <- liftIO $ getExecutablePath
    stubHeaders <- getStubHeaders
    let progInvocation = progName ++ " " ++ unwords args
    rtsIncludeDir <- getIncludePaths
    let instructionList = 
            text "* Add Header Search Paths:"
            $$ nest 4 (text rtsIncludeDir)
            $$ text "* Add Other Linker Flags:"
            $$ nest 4 (text "-liconv")
            $$ text "* Add the following files to XCode:"
            $$ nest 4 (vcat $ map text $ [ moduleInitFile ] ++ stubHeaders)
            $$ text "* Add a \"Run Script\" build phase which occurs before the"
            $$ text "  \"Compile Sources\" phase and calls: "
            $$ nest 4 (text progInvocation) 
    liftIO $ print $ 
        text "You will need to make the following changes in XCode:"
        $$ nest 2 instructionList

writeObjectFiles linkFilePath modules packageIds = do
    objectFiles <- moduleObjectFiles modules
    packageLibFiles <- mapM getPackageLibraryFile packageIds
    let objFiles = objectFiles ++ packageLibFiles
    liftIO $ do
        hPutStrLn stderr $ "Adding object files:"
        mapM_ (hPutStrLn stderr) objFiles
        appendFile linkFilePath
            $ unlines $ objFiles
    
getLinkFilePath = IOError.catchIOError (do
    arch <- getEnv "CURRENT_ARCH"
    variant <- getEnv "CURRENT_VARIANT"
    fmap Just $ getEnv $ "LINK_FILE_LIST_" ++ variant ++ "_" ++ arch)
            (const $ return Nothing)

    
compileAndLoadModules = do
    liftIO $ hPutStrLn stderr "Compiling..."
    success <- load LoadAllTargets
    liftIO $ case success of
            Succeeded -> hPutStrLn stderr "Succeeded."
            Failed -> do
                        hPutStrLn stderr "Failed."
                        exitWith (ExitFailure 1)
    g <- depanal [] False
    let sccs = topSortModuleGraph False g Nothing
    return $ concatMap flattenSCC sccs
        
    

getUniquePackageIds modules = do
    all_deps <- flip mapM modules $ \theModule -> do
                        let m = ms_mod theModule
                        Just info <- getModuleInfo (ms_mod theModule)
                        let Just iface = modInfoIface info
                        return $ map fst $ dep_pkgs $ mi_deps iface
    return $ Set.toList $ Set.fromList 
           $ [rtsPackageId,stringToPackageId "ffi-1.0"] ++ concat all_deps
    
getPackageLibraryFile packageId = do
    dflags <- getSessionDynFlags
    let conf = getPackageDetails (pkgState dflags) packageId
    -- packagehsLibs takes -threaded, etc. into account
    -- for the rts package's libs
    let [libname] = packageHsLibs dflags conf
    let [libdir] = libraryDirs conf
    return $ libdir </> "lib" ++ (libname :: String) <.> "a"


runGHCWithArgsForTargets args action = do
    let locatedArgs = map (mkGeneralLocated "on the command line")
                            args
    -- Static flags are necessary in particular to pick up "-threaded".
    (args', staticFlagWarnings) <- parseStaticFlags locatedArgs
    defaultErrorHandler defaultLogAction $ 
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
        dflags <- getSessionDynFlags
        (dflags2, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags dflags args'
        handleSourceError (\e -> do
            GHC.printException e
            liftIO $ exitWith (ExitFailure 1)) $
          liftIO $ handleFlagWarnings dflags2 
                    $ staticFlagWarnings ++ dynamicFlagWarnings
        setSessionDynFlags dflags2
        let fileArgs = map unLoc fileish_args
        targets <- mapM (flip guessTarget Nothing) fileArgs
        setTargets targets
        action
    
moduleObjectFiles moduleSummaries = do
    targetFiles <- getTargetFiles
    return $ 
        map (ml_obj_file . ms_location) moduleSummaries
        -- stub files
        -- Apparently not needed in ghc-7.2.
        -- ++ map ((++ "_stub.o") . dropExtension) targetFiles

getTargetFiles = do
    targets <- getTargets
    return [f | TargetFile f _ <- map targetId targets]
    

getIncludePaths = do
    dflags <- getSessionDynFlags
    let [rtsIncludeDir] = includeDirs   
                            $ getPackageDetails (pkgState dflags)
                                                rtsPackageId
    return rtsIncludeDir

getStubHeaders = fmap (map ((++"_stub.h") . dropExtension))
                    getTargetFiles

---------
moduleInitFile = "_hs_module_init.c"

moduleInitFileContents moduleSummaries = unlines $
    [ "/* This file has been generated automatically by ghc-xcode."
    , "   You should not edit it directly. */"
    , "#include <HsFFI.h>"
    ]
    ++ ["#include \"" ++ f ++ "\"" | f <- map headerStub moduleSummaries ]
    ++ ["extern void " ++ s ++ "(void);" | s <- stginits] ++ 
    [ "static void library_init(void) __attribute__((constructor));"
    , "static void library_init(void)"
    , "{"
    , "static char *argv[] = { \"PROGNAME\", 0 }, **argv_ = argv;"
    , "static int argc = 1;"
    , ""
    , "hs_init(&argc, &argv_);"
    ]
    ++ ["hs_add_root(" ++ s ++ ");" | s <- stginits] ++
    [ "}"
    , ""
    , "static void library_exit(void) __attribute__((destructor));"
    , "static void library_exit(void)"
    , "{"
    , "hs_exit();"
    , "}"
    ]

  where
    stginits = map stginit moduleSummaries
    stginit m
        = "__stginit_" ++ (fixZEncoding $ moduleNameString $ moduleName $ ms_mod m)
    headerStub m
        = dropExtension (ml_obj_file $ ms_location m) ++ "_stub.h"

fixZEncoding = concatMap $ \c -> case c of
                '.' -> "zi"
                _ -> [c]
