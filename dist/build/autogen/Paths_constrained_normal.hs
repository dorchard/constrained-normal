module Paths_constrained_normal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,5,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/dao29/.cabal/bin"
libdir     = "/home/dao29/.cabal/lib/constrained-normal-1.5.0/ghc-7.6.2"
datadir    = "/home/dao29/.cabal/share/constrained-normal-1.5.0"
libexecdir = "/home/dao29/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "constrained_normal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "constrained_normal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "constrained_normal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "constrained_normal_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
