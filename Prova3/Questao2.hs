module Prova3.Questao2 where

import Gauss (solveLinearEquation)

r1 = 2.8

r2 = 5.4

r3 = 4.4

r4 = 1.9

va = 8

gama = 4

-- Resolution --------------------------------------------------------------------------------------

[g1, g2, g3, g4] = (1 /) <$> [r1, r2, r3, r4]

-- open key
vs = va

-- close key

[v3'', v4''] =
  solveLinearEquation
    [ [g1, - gama],
      [0, g2 + g4 + gama]
    ]
    [- va * g1, - va * g1]

ir4'' = v4'' * g4

is = ir4''

--

rs = vs / is

pmax = - (1 / 4) * (vs * vs) / rs

answer = fromRational pmax