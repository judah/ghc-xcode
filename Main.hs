module Main where

import GHC
import Outputable
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

main = do
    args <- fmap (map $ mkGeneralLocated "on the command line")
                    $ liftIO getArgs
    -- Static flags are necessary in particular to pick up "-threaded".
    (args', staticFlagWarnings) <- parseStaticFlags args
    defaultErrorHandler defaultLogAction $ 
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
        dflags <- getSessionDynFlags
        (dflags2, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags dflags args'
        let fileArgs = map unLoc fileish_args
        handleSourceError (\e -> do
            GHC.printException e
            liftIO $ exitWith (ExitFailure 1)) $
          liftIO $ handleFlagWarnings dflags2 
                    $ staticFlagWarnings ++ dynamicFlagWarnings
        setSessionDynFlags dflags2
        targets <- mapM (flip guessTarget Nothing) fileArgs
        setTargets targets
        success <- load LoadAllTargets
        liftIO $ case success of
            Succeeded -> putStrLn "Succeeded."
            Failed -> do
                        putStrLn "Failed."
                        exitWith (ExitFailure 1)
        g <- depanal [] False
        let sccs = topSortModuleGraph False g Nothing
        let modules = concatMap flattenSCC sccs
        mapM_ (\sum -> liftIO $ putStrLn $ ml_obj_file $ ms_location sum) modules
        liftIO $ mapM_ (\f -> putStrLn $ dropExtension f ++ "_stub.o") fileArgs
        all_deps <- flip mapM modules $ \theModule -> do
                        let m = ms_mod theModule
                        Just info <- getModuleInfo (ms_mod theModule)
                        let Just iface = modInfoIface info
                        return $ map fst $ dep_pkgs $ mi_deps iface
        let unique_pkgs = Set.toList $ Set.fromList 
                            $ [rtsPackageId,stringToPackageId "ffi-1.0"] ++ concat all_deps
        pkgState <- fmap pkgState getSessionDynFlags
        d <- getSessionDynFlags
        flip mapM unique_pkgs $ \pkgId -> do
            let conf = getPackageDetails pkgState pkgId
            -- packagehsLibs takes -threaded, etc. into account
            -- for the rts package's libs
            let [libname] = packageHsLibs d conf
            let [libdir] = libraryDirs conf
            liftIO $ putStrLn $ libdir </> "lib" ++ (libname ::String) <.> "a"


        


