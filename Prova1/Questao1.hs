module Prova1.Questao1 where

import Gauss (solveLinearEquation)

r1 = 6.9 
r2 = 4.7 
r3 = 10 
r4 = 2.1 
r5 = 3.5 
r6 = 3.4 
r7 = 2.9


infixl 5 //
a // b = (a * b) / (a + b)

answer = ((r4 + r5) // r3) + r2