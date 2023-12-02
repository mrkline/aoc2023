{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString qualified as BS
import Data.Char (digitToInt, isDigit)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding(decodeUtf8)
import Data.Text.Read qualified as T
import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

parseInt :: HasCallStack => Text -> Int
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right (v, _) -> v

data Game = Game {
    num :: Int,
    reveals :: [ColorCounts]
} deriving (Show)

data ColorCounts = ColorCounts {
    red :: Int,
    green :: Int,
    blue :: Int
} deriving (Show)

parseGame :: HasCallStack => Text -> Game
parseGame line = let
    -- Every line starts with "Game " - drop that
    sansGame = T.drop (T.length "Game ") line
    -- Then is "<game num/ID>: <reveals>"
    (numStr, rest) = T.breakOn ":" sansGame
    num = parseInt numStr
    reveals = parseReveals $ T.drop (T.length ": ") rest
    in Game{..}

-- Reveals are split by semicolons
parseReveals :: Text -> [ColorCounts]
parseReveals line = parseColorCounts <$> T.splitOn "; " line

parseColorCounts :: HasCallStack => Text -> ColorCounts
parseColorCounts tok = let
    -- Colors are split by commmas, and are "<num> <color>"
    colors = T.splitOn " " <$> T.splitOn ", " tok
    -- Colors can be in different orders, or missing.
    -- See if a given "<num> <color>" is color c:
    maybeColor c t = if last t == c then Just (head t) else Nothing
    -- And fish out the desired color c from the list
    getColor c = listToMaybe $ mapMaybe (maybeColor c) colors
    -- ...If there was no color c, its count is 0.
    orNada c = maybe 0 parseInt $ getColor c
    -- With all that said,
    red = orNada "red"
    green = orNada "green"
    blue = orNada "blue"
    in ColorCounts{..}

-- A game's requirements is the minimum number of red, green, and blue
-- you'd need for all the reveals to be possible.
requirements :: Game -> (Int, ColorCounts)
requirements g = (g.num, maxes) where
    red = maximum $ (.red) <$> g.reveals
    green = maximum $ (.green) <$> g.reveals
    blue = maximum $ (.blue) <$> g.reveals
    maxes = ColorCounts{..}

-- Similarly, a game is possible if the total cubes meets or exceeds its requirements
isPossible :: ColorCounts -> ColorCounts -> Bool
isPossible totals reqs =
    totals.red >= reqs.red &&
    totals.green >= reqs.green &&
    totals.blue >= reqs.blue

-- A game's "power level" for part 2 - its requirements multiplied together
power :: ColorCounts -> Int
power c = c.red * c.green * c.blue

main :: IO ()
main = do
    input <- inputLines "input/day2.txt"
    let games = parseGame <$> input
        idsAndReqs = requirements <$> games
        part1Reqs = ColorCounts 12 13 14
        idIfMeetsReqs (i, r) = if isPossible part1Reqs r then Just i else Nothing
        part1 = sum $ mapMaybe idIfMeetsReqs idsAndReqs
    print part1

    putStrLn ""
    let powers = power . snd <$> idsAndReqs
        part2 = sum powers
    print part2
