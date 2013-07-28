#!/usr/bin/runghc

import System.IO
import System.Environment
import System.Directory
import Control.Monad
import Data.List (sortBy, groupBy)

import qualified Crypto.Hash.SHA1 as C
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL

(</>) :: String -> String -> String
(</>) a b = a ++ "/" ++ b

hashFile :: String -> IO (String, B.ByteString)
hashFile f = do
  -- Lazy hashing, to be able to hash HUGE files (as .exe or big tar files)
  hashed <- withFile f ReadMode $ \h -> do
    dt <- fmap C.hashlazy $ (BL.hGetContents h)
    return $! dt
  return (f, hashed)

merge :: [(Int, (String, B.ByteString))] -> [String] -> IO ()
merge files display = do

  putStrLn "Select a file to keep <n> or skip [s]: "
  string <- getLine
  when (not $ string == "" || string == "s" || string == "q") $ do
  let pIdx = (reads :: ReadS Int) string
  case pIdx of
    (n, _)   : _ | 0 < n && n <= (length files) -> mergei n files
    _            -> putStrLn "Wrong value..." >> mapM_ putStrLn display >> merge files display
  where
    mergei n files = do
      let list = map (\(_, (f, _)) -> f) $ filter (\(m, (_, _)) -> m /= n) files
      mapM_ removeFile list
      return ()


beatiful :: [(String, B.ByteString)] -> IO ()
beatiful sames = do
  putStrLn "The folowing files seams to be the same file :"
  let ziped = zip [1..] sames
  let display = map (\(n, (a, _)) -> (show n) ++ " - " ++ a) ziped
  mapM_ putStrLn $ display
  merge ziped display


work :: String -> IO ()
work folder = do
  putStrLn "Hashing files..."
  -- Get the sorted list of (filename, hash) and group it by hashes
  grouped <- fmap (groupBy (\(_, a) (_, b) -> a == b)) $ computeHashs folder
  -- Filter only duplicated files
  let duplicated = filter (\l -> case l of [a] -> False; _ -> True) grouped
  -- Compute a beatiful display
  mapM_ beatiful duplicated


computeHashs :: String -> IO [(String, B.ByteString)]
computeHashs folder = do
  -- List of files and folder
  -- (also add the folder as a prefix)
  pFiles <- fmap (map (folder </>)) $ getDirectoryContents folder
  -- Keep only the files
  files <- filterM (doesFileExist) pFiles
  -- Hash them
  pairs <- mapM hashFile files
  -- Sort them by hash
  return $ sortBy (\(_, a) (_, b) -> a `compare` b) pairs

main :: IO ()
main = do
  args <- getArgs
  case args of
    [folder] -> work folder
    _        -> putStrLn "checkdoubles <directory>"
