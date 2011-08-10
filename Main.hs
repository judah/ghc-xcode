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
 
import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths
 
import System.Environment
import System.Exit
import System.FilePath
import qualified Data.Set as Set

main = 
    defaultErrorHandler defaultLogAction $ 
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
        args <- fmap (map $ mkGeneralLocated "on the command line")
                    $ liftIO getArgs
        dflags <- getSessionDynFlags
        (dflags2, fileish_args, dynamicFlagWarnings) <- parseDynamicFlags dflags args
        let fileArgs = map unLoc fileish_args
        handleSourceError (\e -> do
            GHC.printException e
            liftIO $ exitWith (ExitFailure 1)) $
          liftIO $ handleFlagWarnings dflags2 dynamicFlagWarnings
        setSessionDynFlags dflags2
        targets <- mapM (flip guessTarget Nothing) fileArgs
        setTargets targets
        -- TODO: print status message for each module as we compile it.
        --       (and then make it more clear where each error message belongs
        --       to.)
        -- TODO: Only compile the files, don't load.
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
        -- liftIO $ putStrLn $ showSDoc $ ppr unique_pkgs

        pkgState <- fmap pkgState getSessionDynFlags
        d <- getSessionDynFlags
        flip mapM unique_pkgs $ \pkgId -> do
            let conf = getPackageDetails pkgState pkgId
            let [libname] = hsLibraries conf
            let [libdir] = libraryDirs conf
            liftIO $ putStrLn $ libdir </> "lib" ++ libname <.> "a"

        -- Still TODO: libHSffi and libHSrts.
        -- TODO: also things like -liconv
 
 {-
        flip mapM_ modules $ \theModule -> do
            let m = ms_mod theModule
            Just info <- getModuleInfo (ms_mod theModule)
            let Just iface = modInfoIface info
            let deps = map fst $ dep_pkgs $ mi_deps iface
            liftIO $ print (showSDoc $ ppr m,showSDoc $ ppr deps)
-}
            

        


{-

./Print-modules ~/Programming/TeXWatch/MacDVI/haskell/TeX/DVI/Foreign.hs -i$HOME/Programming/TeXWatch/MacDVI/haskell -XRecordWildCards -XTemplateHaskell -XScopedTypeVariables -XCPP -XQuasiQuotes 

-}
