module Prova1.Questao4 where

import Gauss (solveLinearEquation)

r1 = 614.4
r2 = 76.9
r3 =  47.7
i1 = 9.8 
i2 = 1.2

[g1, g2, g3] = (1 /) <$> [r1, r2, r3]

infixl 5 //
a // b = (a * b) / (a + b)

-- (g1 + g2 + g3) * v1 = i1 - i2
matrix = 
  [
    [- g1 - g2 - g3]
  ]

result = [i1 - i2]

[v1] = solveLinearEquation matrix result

answer = v1 * i1