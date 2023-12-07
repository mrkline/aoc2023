{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.ST
import Data.ByteString qualified as BS
import Data.Foldable
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Algorithms.Intro

import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

parseInt :: HasCallStack => Text -> Int
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

value :: Char -> Int
value '2' = 2
value '3' = 3
value '4' = 4
value '5' = 5
value '6' = 6
value '7' = 7
value '8' = 8
value '9' = 9
value 'T' = 10
value 'J' = 11
value 'Q' = 12
value 'K' = 13
value 'A' = 14
value w = error $ "Bad card: " ++ [w]

type Cards = IntMap Int

data Hand = Hand {
    line :: Text,
    cards :: Cards,
    bid :: Int
} deriving (Show)

parseHand :: Text -> Hand
parseHand h = Hand { line = head toks, cards, bid } where
    toks = T.words h
    cards = parseCards $ head toks
    bid = parseInt $ last toks

parseCards :: Text -> Cards
parseCards cs = foldl' inc IM.empty $ T.unpack cs where
    inc acc c = IM.insertWith (\_ prev -> prev + 1) (value c) 1 acc

handRank :: HasCallStack => Cards -> Int
handRank c
    | any (== 5) $ IM.elems c = 6 -- Five of a kind
    | any (== 4) $ IM.elems c = 5 -- Four of a kind
    | any (== 3) (IM.elems c) && any (== 2) (IM.elems c) = 4 -- Full house
    | any (== 3) $ IM.elems c = 3 -- Three of a kind
    | length (filter (== 2) $ IM.elems c) == 2 = 2 -- Two pair
    | any (== 2) $ IM.elems c = 1 -- One pair
    | otherwise = 0

compareHands :: Hand -> Hand -> Ordering
compareHands l r = case compare (handRank . cards $ l) (handRank . cards $ r) of
    EQ -> compareSecondary value (T.unpack . line $ l) (T.unpack . line $ r)
    diff -> diff

compareSecondary :: (Char -> Int) -> String -> String -> Ordering
compareSecondary vf (l:ls) (r:rs) = case compare (vf l) (vf r) of
    EQ -> compareSecondary vf ls rs
    diff -> diff
compareSecondary _ [] [] = EQ
compareSecondary _ _ _ = error "hands should be same size"

sortHands :: (Hand -> Hand -> Ordering) -> Vector Hand -> Vector Hand
sortHands ord unsorted = runST $ do
    mv <- V.thaw unsorted
    sortBy ord mv
    sorted <- V.unsafeFreeze mv
    pure sorted

{- To hell with all this, just generate all the possible hands form jokers and use
   the previous ranking.
value2 :: Char -> Int
value2 'J' = 1
value2 '2' = 2
value2 '3' = 3
value2 '4' = 4
value2 '5' = 5
value2 '6' = 6
value2 '7' = 7
value2 '8' = 8
value2 '9' = 9
value2 'T' = 10
-- No Jack, now Joker
value2 'Q' = 12
value2 'K' = 13
value2 'A' = 14
value2 w = error $ "Bad card: " ++ [w]

handRank2 :: HasCallStack => Cards -> Int
handRank2 c = go where
    jokes = fromMaybe 0 $ c IM.!? (value 'J') -- NB: still uses `value` mapping - we didn't reparse
    go
        | (any (== 5) $ IM.map (+jokes) c) = 6 -- Five of a kind
        | any (== 4) $ IM.map (+jokes) c = 5 -- Four of a kind
        | (any (== 3) (IM.map (+jokes) c) && any (== 2) (IM.elems c)) ||
          (any (== 3) (IM.elems c) && any (== 2) (IM.map (+jokes) c))
             = 4 -- Full house

        | any (== 3) $ IM.map (+jokes) c = 3 -- Three of a kind
        | (length (filter (== 2) $ IM.elems c) == 2) ||
          (any (== 2) (IM.elems c) && jokes == 2)
            = 2 -- Two pair

        | any (== 2) $ IM.map (+jokes) c = 1 -- One pair
        | otherwise = 0

compareHands2 :: Hand -> Hand -> Ordering
compareHands2 l r = case compare (handRank2 . cards $ l) (handRank2 . cards $ r) of
    EQ -> compareSecondary value2 (T.unpack . line $ l) (T.unpack . line $ r)
    diff -> diff
-}

main :: IO ()
main = do
    input <- inputLines "input/day7.txt"
    let hands = V.fromList $ parseHand <$> input
        srt = sortHands compareHands hands
    V.mapM_ print $ V.zip srt (V.map (handRank . cards) srt)

    let winnings hs = zipWith (\Hand { bid } i -> bid * i) (V.toList hs) [1..]
    print . sum $ winnings srt
