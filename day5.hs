{-# LANGUAGE OverloadedStrings #-}

import Control.Parallel.Strategies
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HM
import Data.List.Split hiding (chunk)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read qualified as T

import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

chunk :: [Text] -> [[Text]]
chunk = splitWhen (== T.empty)

parseInt :: HasCallStack => Text -> Int
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

parseSeeds :: Text -> [Int]
parseSeeds s = parseInt <$> T.words (T.drop (T.length "seeds: ") s)

mapKind :: [Text] -> (Text, Text)
mapKind c = (head kinds, last kinds) where
    m = T.dropEnd (T.length " map:") $ head c
    kinds = T.splitOn "-to-" m

mapping :: [Int] -> (Int -> Maybe Int)
mapping [dstStart, srcStart, rng] = \x ->
    if x >= srcStart && x < srcStart + rng
            then Just (dstStart + x - srcStart)
            else Nothing
mapping _ = error "Expected three ints"

makeMap :: [Text] -> (Text, (Text, Int -> Int))
makeMap c = (srcKind, (dstKind, joinedMap)) where
    (srcKind, dstKind) = mapKind c
    intLines = T.words <$> tail c :: [[Text]]
    intLines' = (fmap . fmap) parseInt intLines
    mappings = mapping <$> intLines'
    joinedMap x =
        let res = listToMaybe $ mapMaybe ($ x) mappings
        in fromMaybe x res

type Steps = HM.HashMap Text (Text, Int -> Int)

maps :: [[Text]] -> Steps
maps chunks = HM.fromList $ makeMap <$> chunks

grow :: Steps -> Text -> Int -> Int
grow _ "location" x = x
grow s currKind currValue =
    -- trace (printf "%v: %v -> %v: %v" currKind currValue nextKind nextValue) $
    grow s nextKind nextValue where
    (nextKind, f) = s HM.! currKind
    nextValue = f currValue

parseSeeds2 :: Text -> [[Int]]
parseSeeds2 s = ranges where
    ints = parseSeeds s
    pairs = chunksOf 2 ints
    mkRange p = take (last p) [(head p)..]
    ranges = mkRange <$> pairs

main :: IO ()
main = do
    input <- chunk <$> inputLines "input/day5.txt"
    let seeds = parseSeeds $ (head . head) input
        steps = maps $ tail input
        locs = grow steps "seed" <$> seeds
    print $ minimum locs

    -- Lol parallelism
    let seeds2 = parseSeeds2 $ (head . head) input :: [[Int]]
        locs2 = (fmap . fmap) (grow steps "seed") seeds2 :: [[Int]]
        mins = parMap rdeepseq minimum locs2 :: [Int]
    print $ minimum mins
