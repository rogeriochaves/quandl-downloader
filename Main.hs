import System.Environment (getEnv, getArgs)
import Control.Applicative ((<$>))
import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP.Conduit
import Data.String.Utils (replace)

main = do
  args <- getArgs
  mapM downloadStockCsv args
  putStrLn "Done"

downloadStockCsv stock = do
  putStrLn $ "Downloading " ++ stock
  token <- getEnv "QUANDL_KEY"
  request <- getStock stock token
  manager <- newManager tlsManagerSettings
  response <- httpLbs request manager
  return $ responseBody response
  writeFile ("data/" ++ (replace "/" "-" stock) ++ ".csv") $ show $ responseBody response
  putStrLn "Finished"

getStock stock token = setQueryString query <$> url
         where
          url = parseUrl $ "https://www.quandl.com/api/v3/datasets/" ++ stock ++ "/data.csv"
          query = queryParam <$> [("api_key", token),
                                  ("transform", "rdiff")]

queryParam (a, b) = (pack a, (Just (pack b)))
