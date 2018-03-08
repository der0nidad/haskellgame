module Paths_quad (
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

bindir     = "/home/hvatrsh/industry/haskell/projects/quad/.cabal-sandbox/bin"
libdir     = "/home/hvatrsh/industry/haskell/projects/quad/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/quad-0.1.0.0-C14tCRxkpZA46zIDGUAwWj"
datadir    = "/home/hvatrsh/industry/haskell/projects/quad/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/quad-0.1.0.0"
libexecdir = "/home/hvatrsh/industry/haskell/projects/quad/.cabal-sandbox/libexec"
sysconfdir = "/home/hvatrsh/industry/haskell/projects/quad/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "quad_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "quad_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "quad_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "quad_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "quad_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
