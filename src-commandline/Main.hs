module Main
  where
import           CSV2JSON
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.Monoid                ((<>))
import           Options.Applicative

data Arguments = Arguments
  { file :: String
  , sep  :: Maybe Char}

output :: Arguments -> IO()
output (Arguments file Nothing) = do
  csv <- getCSV file ','
  L.putStrLn $ csvToJson csv
output (Arguments file (Just sep)) = do
  csv <- getCSV file sep
  L.putStrLn $ csvToJson csv

run :: Parser Arguments
run = Arguments
     <$> strArgument
          ( metavar "CSV"
         <> help "CSV file" )
     <*> optional (option auto
          ( metavar "SEPARATOR"
         <> long "sep"
         <> short 's'
         <> help "Separator character, e.g ',' (default), '\\t'" ))

main :: IO()
main = execParser opts >>= output
 where
   opts = info (helper <*> run)
     ( fullDesc
    <> progDesc "Convert CSV to JSON"
    <> header "csv2json"
    <> footer "Author: Stéphane Laurent" )
