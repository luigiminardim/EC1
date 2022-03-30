module Prova2.Questao4 where

import Gauss (solveLinearEquation)

r1 = 2.9

r2 = 8.6

r3 = 5.5

r4 = 3.4

_IA = 9.2

_VA = 8.8

_L = 5.2

beta = 9.2

[g1, g2, g3, g4] = (1 /) <$> [r1, r2, r3, r4]

voc =
  let matrixNodal =
        [ [-1, 1, 0, 0],
          [0, g4, 0, 0],
          [0, 0, g1 + g2, - g1],
          [0, 0, - g1 - beta * g2, g1]
        ]
      result = [_VA, _IA, 0, 0]
      [va, vc, ve, vf] = solveLinearEquation matrixNodal result
      vb = ve
   in va - vb

ic =
  let matrixNodal =
        [ [-1, 1, 0],
          [r4, r2 + r3, - r2],
          [0, beta, 1 - beta]
        ]
      result = [_IA, - _VA, 0]
      [i1, i2, i3] = solveLinearEquation matrixNodal result
   in i2

rth = voc / ic

tal = _L / rth