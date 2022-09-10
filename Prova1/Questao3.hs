module Prova1.Questao3 where

import Gauss (solveLinearEquation)

r1 = 1900
r2 = 5000
r3 = 4300
r4 = 10000
r5 = 2500
r6 = 9400
r7 = 83000

infixl 5 //
a // b = (a * b) / (a + b)

deltaToY (r1, r2, r3) = let
  commom = r1 * r2 * r3 / (r1 + r2 + r3)
  r1' = commom / r1
  r2' = commom / r2
  r3' = commom / r3
  in (r1', r2', r3')

answer = let
  (r1', r2', r3') = deltaToY (r1, r2, r3)
  (r5', r6', r7') = deltaToY (r5, r6, r7)
  in r3' + ((r2' + r4 + r7') // (r1' + r6')) + r5'