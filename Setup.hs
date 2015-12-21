import Data.Quandl
import Data.Time (fromGregorian)
import System.Environment

main = do
  token <- getEnv "QUANDL_KEY"
  dataset <- getTableWith (defaultOptions {
                              opSortAscending  = True,
                              opAuthToken      = Just token,
                              opStartDate      = Just (fromGregorian 2015 12 1),
                              opEndDate        = Just (fromGregorian 2015 12 21),
                              opFrequency      = Just Daily,
                              opTransformation = Nothing})
             [("WIKI", "AAPL", Nothing)]
  print $ fmap daData dataset

