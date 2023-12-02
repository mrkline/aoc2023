{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString qualified as BS
import Data.Char (digitToInt, isDigit)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding(decodeUtf8)

inputLines :: FilePath -> IO [Text]
inputLines f = T.lines . decodeUtf8 <$> BS.readFile f

digits :: Text -> [Int]
digits line = digitToInt <$> digits where
    digits = filter isDigit $ T.unpack line

spelledDigits :: Text -> [Int]
spelledDigits "" = []
-- A load of barnacles: eighthree is [8, 3]
spelledDigits line = maybeToList parsedDigit ++ spelledDigits (T.tail line) where
    spellings = [
        ("0", 0),
        ("1", 1),
        ("2", 2),
        ("3", 3),
        ("4", 4),
        ("5", 5),
        ("6", 6),
        ("7", 7),
        ("8", 8),
        ("9", 9),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9)
        ]
    takeDigit :: (Text, Int) -> Maybe Int
    takeDigit (sp, v) = if T.isPrefixOf sp line
        then Just v
        else Nothing
    parsedDigit = listToMaybe $ mapMaybe takeDigit spellings

firstAndLast :: [a] -> (a, a)
firstAndLast xs = (head xs, last xs)

calibration :: (Int, Int) -> Int
calibration (l, r) = l * 10 + r

main :: IO ()
main = do
    input <- inputLines "input/day1.txt"
    let calibrations = calibration . firstAndLast . digits <$> input
    print $ sum calibrations
    putStrLn ""

    let cal2 = calibration . firstAndLast . spelledDigits <$> input
    print $ sum cal2
