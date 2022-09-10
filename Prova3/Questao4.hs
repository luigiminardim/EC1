module Prova3.Questao4 where

import Gauss (solveLinearEquation)

r1 = 9000

r2 = 5000

r3 = 6000

r4 = 51000

ia = 0.0029

va = 3.6

gama = 4

-- Resolution --------------------------------------------------------------------------------------

[g1, g2, g3, g4] = (1 /) <$> [r1, r2, r3, r4]

[v1, v3] =
  solveLinearEquation
    [ [g3 + g4 - g4 / (1 + gama), - g3],
      [- g3, g1 + g3]
    ]
    [ ia,
      0
    ]

answer = fromRational v1