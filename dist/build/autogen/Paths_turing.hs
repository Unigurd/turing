module Paths_turing (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/sigurd/.cabal/bin"
libdir     = "/home/sigurd/.cabal/lib/x86_64-linux-ghc-7.10.3/turing-0.1.0.0-8nl6URMUTb1KXvSBPPqQyh"
datadir    = "/home/sigurd/.cabal/share/x86_64-linux-ghc-7.10.3/turing-0.1.0.0"
libexecdir = "/home/sigurd/.cabal/libexec"
sysconfdir = "/home/sigurd/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "turing_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "turing_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "turing_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "turing_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "turing_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
