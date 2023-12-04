{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as BS
import Data.IntSet (IntSet)
import Data.IntSet qualified as IS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read qualified as T

import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

parseLine :: Text -> (IntSet, IntSet)
parseLine input = (head parsedHalves, last parsedHalves) where
    stripped = T.takeWhileEnd (/= ':') input
    halves = T.splitOn " | " stripped
    toSet half = IS.fromList $ parseInt <$> T.words half
    parsedHalves = toSet <$> halves

parseInt :: HasCallStack => Text -> Int
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

winners :: (IntSet, IntSet) -> Int
winners (wins, haves) = IS.size $ IS.intersection wins haves

points :: Int -> Int
points w = if w == 0 then 0 else 2 ^ (w - 1)

moreCards :: [Int] -> [Int] -> [Int]
moreCards (w:ws) (c:cs) = c : moreCards ws cs' where
    awarded = replicate w c ++ repeat 0
    cs' = zipWith (+) awarded (cs ++ repeat 0)
moreCards _ _ = []

main :: IO ()
main = do
    input <- inputLines "input/day4.txt"
    let parsed = parseLine <$> input
        wins = winners <$> parsed
        part1 = sum $ points <$> wins
    print part1
    putStrLn ""

    print . sum $ moreCards wins (repeat 1)
