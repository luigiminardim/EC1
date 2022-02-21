module Gauss where

import qualified Data.Map as Map
import qualified Data.Set as Set

type Row = [Rational]

type Matrix = [Row]

data Solution = Simple Matrix | Infinite Matrix | Inconsistent deriving (Show)

-- 1. Sort rows by count of leading zeros
-- 2. Make zero in each row at its index position and add it to others making zero in that position from top to bottom
-- 3. Do the same from bottom to the top

quicksort :: (Ord a) => [a] -> (a -> a -> Int) -> [a]
quicksort [] _ = []
quicksort (x : xs) cmp = quicksort lesser cmp ++ [x] ++ quicksort greater cmp
  where
    lesser = [i | i <- xs, cmp x i < 0]
    greater = [i | i <- xs, cmp x i >= 0]

leadingZeros :: Row -> Int
leadingZeros = length . takeWhile (== 0)

-- check if matrix is inconsistent - it will have all zeroes except last column in at least one row
inconsistentMatrix = any $ all (== 0) . reverse . drop 1

infiniteSolutions = any $ all (== 0)

gaussCompareRows :: Row -> Row -> Int
gaussCompareRows r1 r2 = leadingZeros r2 - leadingZeros r1

gaussSortMatrix :: Matrix -> Matrix
gaussSortMatrix = flip quicksort gaussCompareRows

-- gaussConvertMatrix :: [[Fraction]] -> Matrix
-- gaussConvertMatrix = map (map fromInteger)

-- here, guaranteed that r1 has less leading zeros than r2
gaussMakeZero :: Row -> Row -> Row
gaussMakeZero r1 r2 = zipWith (\r1_elt r2_elt -> (r1_elt * factor) + r2_elt) r1 r2
  where
    index = leadingZeros r1
    r1_head = r1 !! index
    r2_head = r2 !! index
    factor = (-1 * r2_head) / r1_head

-- apply the "zeroing head" operation to all the rows except the first one.
-- do this recursively for every row
gaussReduce :: Matrix -> Matrix
gaussReduce [] = []
gaussReduce (r1 : rs) = r1 : gaussReduce (map (gaussMakeZero r1) rs)

gaussFixCoefficients :: Matrix -> Matrix
gaussFixCoefficients [] = []
gaussFixCoefficients (r : rs) = map (/ factor) r : gaussFixCoefficients rs
  where
    index = leadingZeros r
    factor = r !! index

-- converts the matrix row reduced by the Gauss algorithm down to few members to string representation of a result.
-- technically it does not _show_ the results, it also calculates them.
--
-- if a row contains just one number, it is the free member and it will be the resulting variable.
-- if a row contains exactly two numbers, the resulting variable is the free member (last number) over the last coefficient (the first number).
-- if a row contains more numbers, then a simple conversion will be made:
--
-- >>> showVariableValues [3, 4, 5] ["x1", "x2"]
-- "x1 = 5/3 - 4 * x2"
--
-- same as:
--
-- 3x1 + 4x2 = 5
-- 3x1 = 5 - 4x2
-- x1 = (5 - 4x2) / 3
--
-- also, it does not quite work as expected :P
--
showVariableValues :: Row -> [String] -> String
showVariableValues r var_names
  | not (null other_coefficients) = var_str ++ other_vars_str
  | otherwise = var_str
  where
    index = leadingZeros r
    coefficient = r !! index
    value = last r
    raw_row = reverse . drop 1 . reverse $ r -- row coefficients, except the free member
    elements_count = length raw_row
    other_coefficients = filter (\(k, k_idx) -> k /= 0 && k_idx /= index) (zip raw_row [0 .. elements_count])
    subtract_coefficient k = if k < 0 then " + " ++ show (- k) else " - " ++ show k
    other_vars_str = concatMap (\(k, k_idx) -> subtract_coefficient k ++ " * " ++ var_names !! k_idx) other_coefficients
    var_str = var_names !! index ++ " = " ++ show (value / coefficient)

gaussExtractResults :: Matrix -> [String] -> String
gaussExtractResults rows var_names = foldl (\acc row -> showVariableValues row var_names ++ "\n" ++ acc) "" rows

gaussRawSolveMatrix :: Matrix -> Matrix
gaussRawSolveMatrix mat = mat3
  where
    mat1 = gaussReduce mat
    mat2 = gaussReduce $ reverse mat1
    mat3 = gaussFixCoefficients $ reverse mat2

gaussSolveMatrix :: Matrix -> Solution
gaussSolveMatrix mat
  | infiniteSolutions mat1 = Infinite res1'
  | infiniteSolutions mat2 = Infinite res2'
  | inconsistentMatrix mat3 = Inconsistent
  | otherwise = Simple mat3
  where
    mat1 = gaussReduce mat
    mat2 = gaussReduce $ reverse mat1
    mat3 = gaussFixCoefficients $ reverse mat2
    mat1' = filter (not . all (== 0)) mat1
    mat2' = filter (not . all (== 0)) mat2
    res1' = gaussRawSolveMatrix mat1'
    res2' = gaussRawSolveMatrix mat2'

solveLinearEquation :: Matrix -> Row -> [Rational]
solveLinearEquation matrix result = case gaussSolveMatrix combinedMatrix of
  Gauss.Simple m -> last <$> m
  _ -> []
  where
    combinedMatrix = zipWith (\row r -> row ++ [r]) matrix result
