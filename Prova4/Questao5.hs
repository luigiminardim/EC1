module Prova4.Questao5 where

import Gauss (solveLinearEquation)

clamp (a, b) x
  | x < a = a
  | x > b = b
  | otherwise = x

vout (ri, rf) vin = clamp (-50, 50) (- (rf / ri) * vin)

voutResult = (vout (2000, 10000) . vout (1000, 10000)) 2

answer = voutResult