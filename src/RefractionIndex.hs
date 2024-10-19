module RefractionIndex
( calcRefIndex
, calcRefIndexErrors
) where

calcRefIndex :: Float -> [[Float]] -> [Float]
calcRefIndex lambda = map op
  where op (m:d:[]) = 1 + m * lambda / (2 * d)
        op _        = 0

calcRefIndexErrors :: Float -> [[Float]] -> [Float]
calcRefIndexErrors lambdaErr = map op
  where op (m:d:[]) = lambdaErr * m / (2 * d)
        op _        = 0
