{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read qualified as T

import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

records :: [Text] -> [(Int, Int)]
records [times, dists] = zip t d where
    t = parseInt <$> tail (T.words times)
    d = parseInt <$> tail (T.words dists)
records _ = error "Expected two lines"

records2 :: [Text] -> (Int, Int)
records2 [times, dists] = (t, d) where
    t = parseInt . T.concat . tail $ T.words times
    d = parseInt . T.concat . tail $ T.words dists
records2 _ = error "Expected two lines"

parseInt :: HasCallStack => Text -> Int
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

distances :: Int -> [Int]
distances t = go <$> [1..(t - 1)] where
    go held = held * (t - held)

main :: IO ()
main = do
    input <- inputLines "input/day6.txt"
    let recs = records input
        dists = distances . fst <$> recs
        winners :: [Int] -> Int -> [Int]
        winners ds rec = filter (> rec) ds
        winningDists = zipWith winners dists (snd <$> recs)
        waysToWin = length <$> winningDists
    -- mapM_ print waysToWin
    print $ product waysToWin

    let recs2 = records2 input
        dists2 = distances $ fst recs2
        winningDists2 = winners dists2 $ snd recs2
        waysToWin2 = length winningDists2

    print waysToWin2
