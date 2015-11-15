module Paths_bitfinex_conduit (
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
version = Version [0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kevin/.cabal/bin"
libdir     = "/home/kevin/.cabal/lib/i386-linux-ghc-7.10.2.20151105/bitfinex-conduit-0.1-7r3o2SfWjsDDL9ALrhIk53"
datadir    = "/home/kevin/.cabal/share/i386-linux-ghc-7.10.2.20151105/bitfinex-conduit-0.1"
libexecdir = "/home/kevin/.cabal/libexec"
sysconfdir = "/home/kevin/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "bitfinex_conduit_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "bitfinex_conduit_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "bitfinex_conduit_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "bitfinex_conduit_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "bitfinex_conduit_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
