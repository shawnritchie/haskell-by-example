{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_haskell_by_example (
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

bindir     = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/bin"
libdir     = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/lib/x86_64-osx-ghc-8.0.2/haskell-by-example-0.1.0.0-AvWof41KC5XGTmmdiwDUVM"
dynlibdir  = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/lib/x86_64-osx-ghc-8.0.2"
datadir    = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/share/x86_64-osx-ghc-8.0.2/haskell-by-example-0.1.0.0"
libexecdir = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/libexec"
sysconfdir = "/Users/shawnritchie/dev/workspace/haskell/haskell-by-example/.stack-work/install/x86_64-osx/lts-8.2/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_by_example_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_by_example_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_by_example_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_by_example_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_by_example_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_by_example_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
