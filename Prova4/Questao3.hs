module Prova4.Questao3 where

a = 100000

vref = 1.232

vm = 1.227

va = 18

-- Resolução ---------------------------------------------------------------------------------------

clamp (a, b) x
  | x < a = a
  | x > b = b
  | otherwise = x

vout = clamp (- va, va) (a * (vref - vm))

answer = vout