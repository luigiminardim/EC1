module Prova1.Questao3 where

import Gauss (solveLinearEquation)

r1 = 75.8

r2 = 87.4

r3 = 70.3

r4 = 46.3

_IA = 0.13

_VA = 9.2

beta = 3

-- -i1 + i2 = _IA
-- r4 * i1 + (r2 + r3) * i2 - r2 * i3 = - _VA
-- i3 = beta * (i3 - i2) => beta * i2 + (1 - beta) * i3 = 0
matrix =
  [ [r4, r3 + r2, - r2],
    [-1, 1, 0],
    [0, beta, 1 - beta]
  ]

result = [- _VA, _IA, 0]

[i1, i2, i3] = solveLinearEquation matrix result

answer = fromRational i2