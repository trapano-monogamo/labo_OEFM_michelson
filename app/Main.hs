module Main (main) where

-- standard libraries imports
import System.IO
-- import System.Console.ANSI

-- external libraries imports
import Statistics.Distribution
import Statistics.Distribution.StudentT

-- project imports
import Lambda
import RefractionIndex
import WhiteLight
import Sodium



-- ..:: Program ::..

weightedAverage :: [(Float,Float)] -> (Float,Float)
weightedAverage l = ( avg, err )
  where (xs,ws) = unzip $ map (\(x,y) -> (x, 1 / (y**2))) l
        avg = (sum (zipWith (*) xs ws)) / (sum ws)
        err = sqrt (1 / (sum ws))


stdAverage :: [(Float,Float)] -> (Float,Float)
stdAverage l = ( avg, sqrt $ var / (fromIntegral $ length xs) )
  where (xs,_) = unzip l
        avg = (sum xs) / (fromIntegral $ length xs)
        var = (sum $ map (\x -> (x - avg)**2) xs) / ((fromIntegral $ length xs) - 1)


singleTailCriticalTValue :: Float -> Int -> Float
singleTailCriticalTValue significance df = realToFrac $ quantile (studentT $ fromIntegral df) (1 - realToFrac significance)
  -- where alpha = realToFrac $ confLvl + ((1 - confLvl) / 2)

doubleTailCriticalTValue :: Float -> Int -> Float
doubleTailCriticalTValue confidence df = realToFrac $ q1 - q2
  where q1 = quantile (studentT $ fromIntegral df) (realToFrac confidence)
        q2 = quantile (studentT $ fromIntegral df) (1 - realToFrac confidence)


simpleParser :: String -> [[Float]]
simpleParser = filter (not . null) . map (map read) . (map words) . lines


-- ConidenceInterval <confidence level>
-- SignificanceTest <expected value> <significance>
data TTest = ConfidenceInterval Float | SignificanceTest Float Float


-- How can I make this usable for other data analysis programs?
-- Look into algeraic data types to generalize a "parser" and the operators on the dataset
processFile :: String ->                               -- filename
               (String -> [[Float]]) ->                -- parser
               ([[Float]] -> [Float]) ->               -- operation on dataset
               ([[Float]] -> [Float]) ->               -- dataset errors
               ([(Float,Float)] -> (Float,Float)) ->   -- avg+-err statistical estimator
               String ->                               -- measurement units
               Float ->                                -- scale factor
               TTest ->                                -- t-test
               IO (Float, Float)                       -- return (avg,err)
processFile path parseContents processData calcErrors bestEstimate units scaleFactor ttest = do
  {- ASNI colors:
   - default: \ESC[0m
   - red:     \ESC[31m
   - green:   \ESC[32m
   -}
  putStrLn $ "\n[*] Processing " ++ path
  withFile path ReadMode (\handle -> do
    -- read file, get the data and calculate stuff
    contents <- hGetContents handle
    let rawData = parseContents contents
        processedData = processData rawData
        errors = calcErrors rawData
        (avg, err) = bestEstimate $ zip processedData errors
        degOfFreedom = (length processedData) - 1 -- 1 because average is the only constraint
        --interval = confidenceInterval 0.95 degOfFreedom avg err

    -- output stuff
    -- putStrLn "\nRaw data:"
    -- _ <- sequence $ map print $ rawData

    putStrLn "\nResults:"
    _ <- sequence $ map
      (\(x,e) -> putStrLn $ (show $ scaleFactor * x) ++ " +- " ++ (show $ scaleFactor * e) ++ " " ++ units)
      $ zip processedData errors

    putStrLn "\nFinal Measure:"
    putStrLn $ (show $ scaleFactor * avg) ++ " +- " ++ (show $ scaleFactor * err) ++ " " ++ units

    case ttest of
      -- consider implementing a way of testing against multiple significance levels
      SignificanceTest expectedValue significance -> do
        let t0 = (avg - expectedValue) / err
            tc = singleTailCriticalTValue significance degOfFreedom
            p0 = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac t0)
            pc = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -tc)
        putStrLn $ "\nStatistical Significance at " ++ (show $ 100 * significance) ++ "% for expected value of " ++ (show expectedValue) ++ ":"
        putStrLn $ "t0:  P(-inf <= " ++ (show t0) ++ ") = " ++ (show $ 100 * p0) ++ "%"
        putStrLn $ "t_c: P(-inf <= -" ++ (show tc) ++ ") = " ++ (show $ 100 * pc) ++ "%" -- just to check: P(<=tc) should be equal to significance
      ConfidenceInterval confidence -> do
        let tc = doubleTailCriticalTValue confidence degOfFreedom
            lowerBound = avg - tc * err
            upperBound = avg + tc * err
        putStrLn $ "\nMargins of error for confidence level of " ++ (show $ 100 * confidence) ++ "%:"
        putStrLn $ "(" ++ (show $ scaleFactor * lowerBound) ++ ", " ++ (show $ scaleFactor * upperBound) ++ ")"

    -- return results of calculations
    return (avg, err))



-- ..:: Entry Point ::..

-- NOTE: changing t-test significance from 5% to 1% for refraction index produces the same exact result

main :: IO ()
main = do
  (lambda, lambdaErr) <- processFile "./data/misure_lambda.csv"      (simpleParser) (calcLambdas)         (calcLambdaErrors)             (weightedAverage) "nm" (10**6) (SignificanceTest (632.816 * 10**(-6)) 0.05)
  (_, _)              <- processFile "./data/misure_n.csv"           (simpleParser) (calcRefIndex lambda) (calcRefIndexErrors lambdaErr) (weightedAverage) ""   (1)     (SignificanceTest 1.0003 0.01)
  (_, _)              <- processFile "./data/misure_luce_bianca.csv" (simpleParser) (calcWhiteLights)     (calcWhiteLightErrors)         (weightedAverage) "um" (10**3) (ConfidenceInterval 0.95)
  (_, _)              <- processFile "./data/misure_sodio.csv"       (simpleParser) (calcSodiumSeps)      (calcSodiumSepErrors)          (stdAverage)      "A"  (10**7) (SignificanceTest (6 * 10**(-7)) 0.05)
  return ()
