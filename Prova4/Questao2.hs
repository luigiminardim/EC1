module Prova4.Questao2 where

import Data.List (sort)

va = 14

ri = 2000

rf = 25000

v2 = 1.8

-- Resolução  --------------------------------------------------------------------------------------

[gi, gf] = (1 /) <$> [ri, rf]

v1 vout = ((gi + gf) * v2 - gf * vout) / gi

v1s = v1 <$> [- va, va]

answer = maximum v1s