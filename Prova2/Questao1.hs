module Prova2.Questao1 where

infixl 5 //

a // b = (a * b) / (a + b)

l1 = 0.023

l2 = 0.001

l3 = 0.039

l4 = 0.004

l5 = 0.003

l6 = 0.017

leq = l5 // (l6 + (l4 // (l1 + (l2 // l3))))