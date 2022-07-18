module Prova1.Questao5 where

import Gauss (solveLinearEquation)

r1 = 13.2
r2 = 5.4
r3 = 5.6
r4 = 2.2
va = 4.8

-- (r1 + r2) * i1 + (- r2 * i2) * i2 = - 2 * i1
--   =>  (r1 + r2 + 2) * i1 + (- r2 * i2) * i2 = 0
-- (- r2) * i1 + (r2 + r3) * i2 + (- r3) * i3 = 2 * i1
--   =>  (- r2 - 2) * i1 + (r2 + r3) * i2 + (- r3) * i3 = 0
-- (- r3) * i2 + (r4 + r3) * i3 = va
matrix =
  [ [r1 + r2 + 2, - r2, 0],
    [- r2 - 2, r2 + r3, - r3],
    [0, - r3, r4 + r3]
  ]

result = [0, 0, va]

[i1, i2, i3] = solveLinearEquation matrix result

answer = (i2 - i1) * r2