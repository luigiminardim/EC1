module Prova2.Questao2 where

import Gauss (solveLinearEquation)

r1 = 2.1

r2 = 3.4

r3 = 2.4

rL = 6.1

_IA = 0.045

_VA = 9.5

[g1, g2, g3] = (1 /) <$> [r1, r2, r3]

matrixNodal =
  [ [g2 + g3, - g2, 0],
    [- g2, g1 + g2, - g1],
    [0, 0, 1]
  ]

result = [0, _IA, _VA]

solution = solveLinearEquation matrixNodal result