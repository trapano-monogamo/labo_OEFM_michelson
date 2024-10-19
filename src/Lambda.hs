module Lambda
( calcLambdas
, calcLambdaErrors
, micrometerError
) where

import LabParameters

calcLambdas :: [[Float]] -> [Float]
calcLambdas = map op
  where op (x1:x2:n:[]) = 2 * (x2 - x1) / (5 * n)
        op _            = 0

calcLambdaErrors :: [[Float]] -> [Float]
calcLambdaErrors = map op
  where op (_:_:n:[]) = (2 / n) * (micrometerError * sqrt 2)
        op _            = 0
