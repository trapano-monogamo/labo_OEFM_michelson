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

confidenceInterval :: Float -> Int -> Float -> Float -> (Float,Float)
confidenceInterval confLvl degOfFreedom avg avgErr = (lowerBound, upperBound)
  where tc = singleTailCriticalTValue confLvl degOfFreedom                       -- WRONG! you need double tail
        lowerBound = avg - tc * avgErr
        upperBound = avg + tc * avgErr


simpleParser :: String -> [[Float]]
simpleParser = filter (not . null) . map (map read) . (map words) . lines


data TTest = ConfidenceInterval Float | SignificanceTest Float Float


-- How can I make this usable for other data analysis programs?
-- Look into algeraic data types to generalize a "parser" and the operators on the dataset
processFile :: String ->                               -- filename
               (String -> [[Float]]) ->                -- parser
               ([[Float]] -> [Float]) ->               -- operation on dataset
               ([[Float]] -> [Float]) ->               -- dataset errors
               ([(Float,Float)] -> (Float,Float)) ->   -- avg+-err statistical estimator
               String ->                               -- measurement units
               TTest ->                                -- t-test
               IO (Float, Float)                       -- return (avg,err)
processFile path parseContents processData calcErrors bestEstimate units ttest = do
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
      (\(x,e) -> putStrLn $ (show x) ++ " +- " ++ (show e) ++ " " ++ units)
      $ zip processedData errors

    putStrLn "\nFinal Measure:"
    putStrLn $ (show avg) ++ " +- " ++ (show err) ++ " " ++ units

    case ttest of
      -- consider implementing a way of testing against multiple significance levels
      SignificanceTest expectedValue significance -> do
        let t0 = (avg - expectedValue) / err
            tc = singleTailCriticalTValue significance degOfFreedom
            p0 = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac t0)
            pc = cumulative (studentT $ fromIntegral degOfFreedom) (realToFrac $ -tc)
        putStrLn $ "\nStatistical Significance at " ++ (show $ 100 * significance) ++ "% for expected value of " ++ (show expectedValue)
        putStrLn $ "t0:  P(-inf <= " ++ (show t0) ++ ") = " ++ (show $ 100 * p0) ++ "%"
        putStrLn $ "t_c: P(-inf <= -" ++ (show tc) ++ ") = " ++ (show $ 100 * pc) ++ "%" -- just to check: P(<=tc) should be equal to significance
      ConfidenceInterval _ -> putStrLn "\nConfidence Inerval: Not yet implemented"
        -- putStrLn "\nConfidence Level:"
        -- putStrLn $ (show tc) ++ " " ++ (show $ 100 * confLvl) ++ "% " ++ (show interval)

    -- return results of calculations
    return (avg, err))



-- ..:: Entry Point ::..

main :: IO ()
main = do
  (lambda, lambdaErr) <- processFile "./data/misure_lambda.csv"      (simpleParser) (calcLambdas)         (calcLambdaErrors)             (weightedAverage) "mm" (SignificanceTest (632.816 * 10**(-6)) 0.05)
  (_, _)              <- processFile "./data/misure_n.csv"           (simpleParser) (calcRefIndex lambda) (calcRefIndexErrors lambdaErr) (weightedAverage) ""   (SignificanceTest 1.0003 0.05)
  (_, _)              <- processFile "./data/misure_luce_bianca.csv" (simpleParser) (calcWhiteLights)     (calcWhiteLightErrors)         (weightedAverage) "mm" (ConfidenceInterval 0.95)
  (_, _)              <- processFile "./data/misure_sodio.csv"       (simpleParser) (calcSodiumSeps)      (calcSodiumSepErrors)          (stdAverage)      "mm" (SignificanceTest (6 * 10**(-7)) 0.05)
  return ()
