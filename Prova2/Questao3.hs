module Prova2.Questao3 where

import Gauss (solveLinearEquation)

r1 = 1.9

r2 = 5.6

r3 = 5.5

r4 = 1.2

_IA = 0.016

_VA = 7.4

beta = 9

matrixNodal =
  [ [-1, 1, 0],
    [r4, r2 + r3, - r2],
    [0, beta, 1 - beta]
  ]

result = [_IA, - _VA, 0]

solution = solveLinearEquation matrixNodal result