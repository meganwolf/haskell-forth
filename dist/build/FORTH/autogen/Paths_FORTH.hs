{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_FORTH (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/meganwolf/.cabal/bin"
libdir     = "/Users/meganwolf/.cabal/lib/x86_64-osx-ghc-8.6.3/FORTH-0.1.0.0-KM5rT0wYIKWCLxci8IAJ5J"
dynlibdir  = "/Users/meganwolf/.cabal/lib/x86_64-osx-ghc-8.6.3"
datadir    = "/Users/meganwolf/.cabal/share/x86_64-osx-ghc-8.6.3/FORTH-0.1.0.0"
libexecdir = "/Users/meganwolf/.cabal/libexec/x86_64-osx-ghc-8.6.3/FORTH-0.1.0.0"
sysconfdir = "/Users/meganwolf/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FORTH_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FORTH_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FORTH_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FORTH_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FORTH_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FORTH_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
