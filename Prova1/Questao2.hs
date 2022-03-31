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

transformYToDelta (ra, rb, rc) =
  let sums = ra * rb + ra * rc + rb * rc
  in (sums / ra, sums / rb, sums / rc)

req =
  let (r4', r6', r8') = transformYToDelta (r4, r6, r8)
   in (r1 + r2) // (r3 // r6') // ((r5 // r8') + (r7 // r4'))

answer = fromRational  req