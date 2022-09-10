module Prova3.Questao5 where

import Gauss (solveLinearEquation)

r1 = 96

r2 = 120

r3 = 32

r4 = 30

va = 20

i0 = 0.2

-- Resolution --------------------------------------------------------------------------------------

[g1, g2, g3, g4] = (1 /) <$> [r1, r2, r3, r4]

solve va i0 =
  let [v1, v2, v3] =
        solveLinearEquation
          [ [1, -1, 0],
            [g1 + g2, g3 + g4, - g1 - g3],
            [- g1, - g3, g1 + g3]
          ]
          [va, 0, i0]
   in v2

answer = (fromRational (solve 0 i0), fromRational (solve va 0))