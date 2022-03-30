module Prova1.Questao1 where

import Gauss (solveLinearEquation)

_I1 = 0.13

r1 = 92.7

r2 = 3.5

r3 = 4.1

gama = 2

matrix =
  [ [r1, r2, 0, 0],
    [-1, 1, 0, 0],
    [0, 0, r2, r3],
    [gama, 0, 1, -1]
  ]

result = [0, _I1, 0, 0]

solution = solveLinearEquation matrix result