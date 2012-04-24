module Paths_Exodus (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/g/work/Exodus/cabal-dev//bin"
libdir     = "/home/g/work/Exodus/cabal-dev//lib/Exodus-0.0.0/ghc-7.4.1"
datadir    = "/home/g/work/Exodus/cabal-dev//share/Exodus-0.0.0"
libexecdir = "/home/g/work/Exodus/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Exodus_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Exodus_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Exodus_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Exodus_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
