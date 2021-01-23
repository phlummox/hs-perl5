

{-# LANGUAGE CPP #-}

import Distribution.Simple
import Distribution.Verbosity
import Distribution.Simple.PreProcess
import Distribution.Simple.Utils
import Distribution.PackageDescription
import Distribution.Simple.LocalBuildInfo
import System.Process (rawSystem)
import System.FilePath ((</>))
import Control.Monad
import System.Directory   (removeFile, doesFileExist
                          ,doesDirectoryExist, removeDirectoryRecursive)
import Distribution.Simple.Setup
#ifdef CABAL_PARSEC
import Distribution.PackageDescription.Parsec
import Distribution.PackageDescription.Parse (readHookedBuildInfo)
#else
import Distribution.PackageDescription.Parse
#endif

main :: IO ()
--main = writeBuildInfo >> defaultMainWithHooks defaultUserHooks
main = writeBuildInfo >> defaultMainWithHooks myUserHooks
     where
     writeBuildInfo = rawSystem "bash" ["perl_configure.sh"]

     -- more or less duplicates behaviour of old "defaultUserHooks"
     myUserHooks = autoconfUserHooks {
          confHook = \pkg flags -> do
                       let verbosity = fromFlag (configVerbosity flags)
                       warn verbosity
                         "defaultUserHooks in Setup script is deprecated."
                       confHook autoconfUserHooks pkg flags,
          postConf = oldCompatPostConf
          }
        where oldCompatPostConf args flags pkg_descr lbi
                  = do let verbosity = fromFlag (configVerbosity flags)
                       pbi <- getHookedBuildInfo verbosity
                       let pkg_descr' = updatePackageDescription pbi pkg_descr
                           lbi' = lbi { localPkgDescr = pkg_descr' }
                       postConf simpleUserHooks args flags pkg_descr' lbi'

              getHookedBuildInfo :: Verbosity -> IO HookedBuildInfo
              getHookedBuildInfo verbosity = do
                maybe_infoFile <- defaultHookedPackageDesc
                case maybe_infoFile of
                  Nothing       -> return emptyHookedBuildInfo
                  Just infoFile -> do
                    info verbosity $ "Reading parameters from " ++ infoFile
                    readHookedBuildInfo verbosity infoFile



