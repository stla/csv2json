
-- faire readFile ou readLines
-- match date:
-- Prelude Text.Regex.Posix> "01-12" =~ "[0-9]{1,2}[/|-][0-9]{1,2}$" :: Bool

-- http://stackoverflow.com/a/2488528/1100107

module CSV2JSON
  where
import           Control.Arrow                 ((&&&))
import           Data.Aeson
import           Data.ByteString.Lazy.Internal (ByteString)
import           Data.CSV.Conduit
import qualified Data.HashMap.Strict.InsOrd    as M
import           Data.List                     (transpose)
import           Data.Scientific
import           Data.Text                     (pack)
import qualified Data.Vector                   as V

isNumber :: String -> Bool
isNumber string =
  case test of
    []        -> False
    [(_, "")] -> True
    _         -> False
  where test = reads string :: [(Double, String)]

isBool :: String -> Bool
isBool string =
  case string of
    "TRUE"  -> True
    "FALSE" -> True
    _       -> False

cellToValue :: String -> Value
cellToValue string
  | isBool string =
    if string == "TRUE" then Bool True else Bool False
  | isNumber string =
    Number (read string :: Scientific)
  | string == "" =
    Null
  | otherwise =
    String (pack string)

splitCSV :: [[String]] -> ([String], [[Value]])
splitCSV csv = (head csv, map (map cellToValue) $ transpose $ tail csv)

csvToList :: [[String]] -> [(String, [Value])]
csvToList csv = map ((!!) header &&& (!!) body) [0 .. length header -1]
  where (header, body) = splitCSV csv

csvToJson :: [[String]] -> ByteString
csvToJson = encode . M.fromList . csvToList

getCSV :: FilePath -> Char -> IO [[String]]
getCSV csv sep = do
  x <- readCSVFile settings csv
  return $ V.toList x
    where settings = CSVSettings {csvSep = sep, csvQuoteChar = Just '"'}
