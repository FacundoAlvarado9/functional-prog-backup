module Traverse where

import Control.Monad(filterM, mapM_)
import Control.Exception(catch)
import System.Directory
import System.FilePath((</>))
import Data.List((\\))
import System.Environment(getArgs)

travFS :: (FilePath-> IO ())-> FilePath-> IO ()
travFS action p = catch (do
    cs<- getDirectoryContents p
    let cp = map (p </>) (cs \\ [".", ".."])
    dirs  <- filterM doesDirectoryExist cp
    files <- filterM doesFileExist cp
    mapM_ action files
    mapM_ (travFS action) dirs)
  (\e -> putStrLn $ "ERROR: "++ show (e :: IOError))

-- travFS' :: (FilePath-> IO ())-> FilePath-> IO ()
-- travFS' action p = catch (
--   getDirectoryContents p
--   >>= \cs -> (map (p </>) (cs \\ [".", ".."]))
--   >>= \cp -> filterM doesFileExist cp
--   >>= \files -> mapM_ action files
--   >> filterM doesDirectoryExist cp -- will it still find variable 'cp' in scope?
--   >>= \files -> mapM_ action files)
--   (\e -> putStrLn $ "ERROR: "++ show (e :: IOError))

sequence' (x:xs) =
  x >>= \y ->
  sequence' xs >>= \ys -> 
  return (y:ys)