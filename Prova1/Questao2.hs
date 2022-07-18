module Prova1.Questao2 where

r1 = 578.4
r2 = 5.1
r3 = 308.4
ia = 0.2

infixl 5 //
a // b = (a * b) / (a + b)

req = r1 // (r2 + r3)

i3 = ia * (req / (r2 + r3))

v3 = i3 * r3

answer = v3