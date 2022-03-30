module Prova1.Questao3 where

import Gauss (solveLinearEquation)

r1 = 75.8

r2 = 87.4

r3 = 70.3

r4 = 46.3

_IA = 0.13

_VA = 9.2

beta = 3

matrix =
  [ [r4, r3 + r2, - r2],
    [-1, 1, 0],
    [0, - beta, beta + 1]
  ]

result = [- _VA, _IA, 0]

is = solveLinearEquation matrix result

iR2 = is !! 2 - is !! 1