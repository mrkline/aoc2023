{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

import Data.ByteString qualified as BS
import Data.Char (isDigit)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read qualified as T
import GHC.Stack

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

parseInt :: HasCallStack => Text -> (Int, Text)
parseInt t = case T.decimal t of
    Left e -> error (T.unpack t <> ": " <> e)
    Right v -> v

data PartNo = PartNo {
    pnum :: Int,
    row :: Int,
    leftIdx :: Int,
    rightIdx :: Int -- Inclusive, makes adjacent nice and symmetrical
} deriving (Show)

parsePartNos :: [Text] -> [PartNo]
parsePartNos input = concatMap (parsePartNosInLine 0) indexedLines where
    indexedLines = zip [0..] input

parsePartNosInLine :: Int -> (Int, Text) -> [PartNo]
parsePartNosInLine startIdx (row, line) = case T.findIndex isDigit line of
    Nothing -> []
    Just i -> let -- We've got a digit at index i
        (pnum, rest) = parseInt $ T.drop i line -- Drop everything before it.
        leftIdx = startIdx + i -- Our dumb part number bounds
        rightIdx = leftIdx + length (show pnum) - 1
        -- That part plus parts in the rest of hte line
        in PartNo{..} : parsePartNosInLine (rightIdx + 1) (row, rest)


data Symbol = Symbol {
    symbol :: Char,
    x :: Int,
    y :: Int
} deriving (Show)

parseSymbols :: [Text] -> [Symbol]
parseSymbols input = concatMap parseSymbolsInLine indexedLines where
    indexedLines = zip [0..] input

parseSymbolsInLine :: (Int, Text) -> [Symbol]
parseSymbolsInLine (y, line) = mapMaybe maybeSymbol indexedChars where
    indexedChars = zip [0..] (T.unpack line)
    -- If it's not a digit and it's not '.', it's a symbol.
    maybeSymbol (x, c) = if not (isDigit c) && c /= '.' then Just (Symbol c x y) else Nothing

adjacent :: PartNo -> Symbol -> Bool
adjacent p Symbol{x, y} =
    p.row >= y - 1 &&
    p.row <= y + 1 &&
    x >= p.leftIdx - 1 &&
    x <= p.rightIdx + 1

-- A symbol is a gear if it's '*' and has >= 2 adjacent parts numbers.
-- The gear ratio is the product of those adjacent part numbers.
asGear :: [PartNo] -> Symbol -> Maybe Int
asGear parts s@Symbol { symbol = '*' } = let
    adjacentParts = filter (flip adjacent s) parts
    in if length adjacentParts >= 2
        then Just (product $ (.pnum) <$> adjacentParts)
        else Nothing

asGear _ _ = Nothing

main :: IO ()
main = do
    input <- inputLines "input/day3.txt"
    let partNumbers = parsePartNos input
        symbols = parseSymbols input
        adjacents = filter (\n -> any (adjacent n) symbols) partNumbers
    print . sum $ (.pnum) <$> adjacents

    let gears = mapMaybe (asGear partNumbers) symbols
    print $ sum gears
