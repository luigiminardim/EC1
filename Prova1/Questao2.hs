module Prova1.Questao2 where

r1 = 37.6

r2 = 2.7

r3 = 8.1

r4 = 7.4

r5 = 2.2

r6 = 2.5

r7 = 6

r8 = 6.3

infixl 5 //

a // b = (a * b) / (a + b)

req =
  let sum486 = r4 + r8 + r6
   in ((r1 + r2) // r3 // (sum486 / r6)) // ((sum486 / r8) + (r7 // (sum486 / r4)))