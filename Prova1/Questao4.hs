module Prova1.Questao4 where

import Gauss (solveLinearEquation)

r1 = 25.6

r2 = 9.1

r3 = 6.9

r4 = 5.1

r5 = 4.9

_IB = 0.088

_VA = 6.7

gama = 4

[g1, g2, g3, g4, g5] = (1 /) <$> [r1, r2, r3, r4, r5]

-- -v1 + v2 = _VA
-- (g1 + g2) * v1 + (g3 + g4) * v2 - g4 * v3 + (- g2 - g3) * v4 = gama * ((v1 - v4) * g2)
-- => (g1 + g2 - gama * g2) * v1 + (g3 + g4) * v2 - g4 * v3 + (- g2 - g3 + gama * g2) * v4 = 0
-- - g4 * v2 + (g4 + g5) * v3 = - _IB
-- - g2 * v1 - g3 * v2 + (g2 + g3) * v4 = _IB
matrix =
  [ [-1, 1, 0, 0],
    [g1 + g2 - gama * g2, g3 + g4, - g4, - g2 - g3 + gama * g2],
    [0, - g4, g4 + g5, 0],
    [- g2, - g3, 0, g2 + g3]
  ]

result = [_VA, 0, - _IB, _IB]

[v1, v2, v3, v4] = solveLinearEquation matrix result

answer = fromRational v2