module Prova1.Questao1 where

import Gauss (solveLinearEquation)

_I1 = 0.13

r1 = 92.7

r2 = 3.5

r3 = 4.1

gama = 2

-- -i1 + i2 = _I1
-- i1 * r1 + i2 * r2 - i3 * r2 = 0
-- i3 - i4 = gama * _I_r1 => i3 - i4 = gama * -i1 => gama * i1 + i3 - i4 = 0
-- -i2 * r2 + i3 * r2 + i4 * r3 = 0
matrix =
  [ [r1, r2, -r2, 0],
    [-1, 1, 0, 0],
    [0, -r2, r2, r3],
    [gama, 0, 1, -1]
  ]

result = [0, _I1, 0, 0]

[i1, i2, i3, i4] = solveLinearEquation matrix result

_Ix = -i1

answer = fromRational _Ix