module Prova4.Questao1 where

r1 = 5000

r2 = 9000

v2 = 5.4

vout = -2.2

-- Resolution --------------------------------------------------------------------------------------

[g1, g2] = (1 /) <$> [r1, r2]

v1 = (g1 * v2 - g2 * v2) / g1

answer = v1