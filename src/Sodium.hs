module Sodium
( calcSodiumSeps
, calcSodiumSepErrors
) where

import LabParameters

calcSodiumSeps :: [[Float]] -> [Float]
calcSodiumSeps = map op
  where op (m:x1:x2:[]) = (avgSodiumLambda ** 2) * m * 5 / (2 * (x2 - x1))
        op _            = 0

calcSodiumSepErrors :: [[Float]] -> [Float]
calcSodiumSepErrors = map op
  where op (m:x1:x2:[]) = (micrometerError * sqrt 2) * m * (avgSodiumLambda ** 2) * 5 / (2 * (x2 - x1)) 
        op _            = 0
