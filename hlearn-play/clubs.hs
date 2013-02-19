
import Control.Lens
import Data.Csv
import Data.Time.LocalTime
import Data.Time.Parse
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8  as BS

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Gnuplot.Distributions

main = do
    -- 2012-10-11T04:32:01.281+02,27749909976,FAIL,65507
    -- 2012-10-11T04:32:04.034+02,27849203106,FAIL,65507

    ccdata  <- loadDataFile "data/ace_2012_10_11_cc"
    vcdata  <- loadDataFile "data/ace_2012_10_11_vc"
    mtndata <- loadDataFile "data/ace_2012_10_11_mtn"

    -- let list_cc_result = fmap (\row -> row^._3) ccdata

    -- let cat_cc = train list_cc_result :: Categorical String Double

    putStrLn $ "All cc data : " ++ (show $ length ccdata)
    -- putStrLn $ "more cc data : " ++ (show $ take 20 list_cc_result)

    -- plotDistribution (genPlotParams "cc_res" cat_cc) cat_cc


{-
    let list_usa    = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="USA"   ) rawdata
    let list_uk     = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="UK"    ) rawdata 
    let list_france = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="France") rawdata 
    let list_russia = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="Russia") rawdata 
    let list_china  = fmap (\row -> row^._4) $ filter (\row -> (row^._1)=="China" ) rawdata

    putStrLn $ "List of American nuclear weapon sizes = " ++ show (sum list_usa)

    let list_all = list_usa ++ list_uk ++ list_france ++ list_russia ++ list_china
    putStrLn $ "Number of nukes in the whole world = " ++ show (length list_all)

    let cat_usa = train list_usa :: Categorical Int Double
    let cat_uk = train list_uk :: Categorical Int Double
    let cat_france = train list_france :: Categorical Int Double
    let cat_russia = train list_russia :: Categorical Int Double
    let cat_china = train list_china :: Categorical Int Double

    let cat_allA = train list_all :: Categorical Int Double
    let cat_allB = cat_usa <> cat_uk <> cat_france <> cat_russia <> cat_china

    plotDistribution (genPlotParams "cal_allA" cat_allA) cat_allA

    let list_bomber = fmap (\row -> row^._4) $ filter (\row -> (row^._2)=="Bomber") rawdata
    let cat_bomber = train list_bomber :: Categorical Int Double

    let cat_survivable = cat_allB <> (inverse cat_bomber)


    let kdeparams = KDEParams
         { bandwidth    = Constant 20.0
         , samplePoints = genSamplePoints
                0.0       -- minimum
                4000.0    -- maximum
                4000.0    -- number of samples
         , kernel       = KernelBox Gaussian
         } :: KDEParams Double

    let kde_usa     = train' kdeparams list_usa      :: KDE Double

    plotDistribution (genPlotParams "kde_usa" kde_usa) kde_usa
    -- plotDistribution (genPlotParams "kde_all" kde_all_parA) kde_all_parA

-}

    putStrLn "Done"

-- ---------------------------------------------------------------------

parseTimestamp :: String -> Maybe (LocalTime)
parseTimestamp str = res
  where
    {-
    from the R version

    bill$timestamp <- strptime( format="%Y-%m-%dT%H:%M:%OS%z"
                               , tz="Africa/Johannesburg"
                               , paste(bill$timestamp,"00", sep=""))
    -}

    res = case (strptime fmt (str ++ "00")) of
      Just (lt,_) -> Just lt
      Nothing -> Nothing

    fmt = "%Y-%m-%dT%H:%M:%OS%z"

t = parseTimestamp "2012-10-11T04:32:01.281+02"

getTimestamp str = ts
  where
    Just ts = parseTimestamp str

-- ---------------------------------------------------------------------

data BillData = BD { bdTs :: LocalTime,
                     bdMsisdn :: Msisdn,
                     bdResult :: BillResult,
                     bdOperator :: MobileOperator
                   }
type Msisdn         = String
type BillResult     = String
type MobileOperator = String

-- 2012-10-11T04:32:01.281+02,27749909976,FAIL,65507
-- 2012-10-11T04:32:04.034+02,27849203106,FAIL,65507
loadDataFile
  -- :: FilePath -> IO [(LocalTime, Msisdn, BillResult, MobileOperator)]
  :: FilePath -> IO [BillData]
loadDataFile fname = do
  Right fdata <- fmap (fmap V.toList . decode True) $ BS.readFile fname
        :: IO (Either String [(String, Msisdn, BillResult, MobileOperator)])

  let fdata' = map (\(ts,m,r,nw) -> (BD (getTimestamp ts) m r nw)) fdata
  return fdata'

-- ---------------------------------------------------------------------

-- calcOperator

-- ---------------------------------------------------------------------


