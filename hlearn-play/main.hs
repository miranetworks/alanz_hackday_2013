
import Control.Lens
import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy.Char8  as BS

import HLearn.Algebra
import HLearn.Models.Distributions
import HLearn.Gnuplot.Distributions

main = do
    Right rawdata <- fmap (fmap V.toList . decode True) $ BS.readFile "nukes-list.csv"
        :: IO (Either String [(String, String, String, Int)])
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

