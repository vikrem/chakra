-- All this junk is to just add two paths to library-paths

import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.Simple.LocalBuildInfo
    ( LocalBuildInfo
    , configFlags
    , localPkgDescr
    )
import Data.Maybe
    ( fromJust
    , fromMaybe
    )
import System.Directory
    ( doesDirectoryExist
    , getCurrentDirectory
    , removeDirectoryRecursive
    )
import qualified Distribution.PackageDescription as PD

main = defaultMainWithHooks simpleUserHooks {
  confHook = libConfHook
  }

libConfHook :: (PD.GenericPackageDescription, PD.HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
libConfHook (description, buildInfo) flags = do
    localBuildInfo <- confHook simpleUserHooks (description, buildInfo) flags
    let packageDescription = localPkgDescr localBuildInfo
        library = fromJust $ PD.library packageDescription
        libraryBuildInfo = PD.libBuildInfo library
    dir <- getCurrentDirectory
    return localBuildInfo {
        localPkgDescr = packageDescription {
            PD.library = Just $ library {
                PD.libBuildInfo = libraryBuildInfo {
                    PD.extraLibDirs = (map (dir ++) extra_dirs) ++ PD.extraLibDirs libraryBuildInfo
                }
            }
        }
    }
    where
      extra_dirs = [
        "/vendor/ChakraCoreFiles_1_8_2_OSX/lib",
        "/vendor/ChakraCoreFiles_1_8_2_Linux/lib"
        ]
