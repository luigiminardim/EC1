{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Exp3 where

import qualified Gauss

matrixNodal :: [[Rational]]
matrixNodal =
  map
    (map (sum . map (1 /)))
    [ [[100, 1000, 4700], [-1000]],
      [[-1000], [1000, 2000, 2200]]
    ]

correntes :: Fractional a => a -> a -> [a]
correntes v1 v2 = [v1 / 100, v2 / 2000]

combinacaoFontes :: [(Rational, Rational)]
combinacaoFontes = [(6, 0), (0, 4), (6, 4)]

solution :: Rational -> Rational -> [Rational]
solution v1 v2 =
  let v_a = v1
      v_c = v2
      v_e = 0
      [v_b, v_d] = Gauss.solveLinearEquation matrixNodal (correntes v1 v2)
   in [v_a, v_b, v_c, v_d, v_e]

tensoesResistor2200 :: [Rational]
tensoesResistor2200 =
  let vR2200 [v_a, v_b, v_c, v_d, v_e] = v_d - v_e
   in vR2200 . uncurry solution <$> combinacaoFontes

correntesResistor1000 :: [Rational]
correntesResistor1000 =
  let iR1000 [v_a, v_b, v_c, v_d, v_e] = (v_d - v_b) / 1000
   in iR1000 . uncurry solution <$> combinacaoFontes