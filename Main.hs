import System.Environment (getEnv, getArgs)
import Control.Applicative ((<$>))
import Data.ByteString.Char8 (pack, unpack)
import Network.HTTP.Conduit
import Data.String.Utils (replace)
import Data.ByteString.Internal (ByteString)
import Control.Monad.Catch (MonadThrow)

type Stock = String
type Token = String

main = do
  args <- getArgs
  manager <- newManager tlsManagerSettings
  mapM (downloadStockCsv manager) args
  putStrLn "Done"

downloadStockCsv :: Manager -> Stock -> IO ()
downloadStockCsv manager stock = do
  putStrLn $ "Downloading " ++ stock
  response <- getStock manager stock
  writeFile (filePathFor stock) response
  putStrLn "Finished"

getStock :: Manager -> Stock -> IO String
getStock manager stock = do
  request <- getStockRequestWithEnvToken stock
  response <- httpLbs request manager
  return $ show (responseBody response)

getStockRequestWithEnvToken :: Stock -> IO Request
getStockRequestWithEnvToken stock = do
  token <- getEnv "QUANDL_KEY"
  getStockRequest stock token

getStockRequest :: (MonadThrow f, Functor f) => Stock -> Token -> f Request
getStockRequest stock token = setQueryString query <$> url
         where
          url = parseUrl $ "https://www.quandl.com/api/v3/datasets/" ++ stock ++ "/data.csv"
          query = queryParam <$> [("api_key", token),
                                  ("transform", "rdiff")]

queryParam :: (String, String) -> (ByteString, Maybe ByteString)
queryParam (a, b) = (pack a, Just (pack b))

filePathFor :: Stock -> String
filePathFor stock = "data/" ++ (replace "/" "-" stock) ++ ".csv"
