module WhiteLight
( calcWhiteLights
, calcWhiteLightErrors
) where

import LabParameters

calcWhiteLights :: [[Float]] -> [Float]
calcWhiteLights = map op
  where op (x1:x2:[]) = 2 * (x2 - x1) / 5
        op _          = 0

calcWhiteLightErrors :: [[Float]] -> [Float]
calcWhiteLightErrors = map op
  where op (x1:x2:[]) = 2 * micrometerError
