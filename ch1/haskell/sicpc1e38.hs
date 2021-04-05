-- The solution of exercise 1.38
-- In 1737, the Swiss mathematician Leonhard Euler published a memoir De
-- Fractionibus Continuis, which included a continued fraction expansion
-- for `e - 2`, where e is the base of the natural logarithms. In this
-- fraction, the N_i are all 1, and the D_i are successively 1, 2, 1, 1,
-- 4, 1, 1, 6, 1, 1, 8, .... Write a program that uses your cont-frac
-- procedure from exercise 1.37 to approximate `e`, based on Euler's
-- expansion.
--
-- -------- (above from SICP)
--

-- The `cont-frac` procedure as a recursive process
contFrac
  :: (Num a, Eq a, Fractional b) =>
     (a -> b) -> (a -> b) -> a -> b
contFrac n d k =
  let iter counter =
        n counter / (d counter +
                      if k == counter
                      then 0
                      else iter (counter + 1))
  in iter 1

-- The iterative process to compute the approximate value of the base of
-- the natural logarithms `e` using Eular's formula.
eularExpansion :: (Integral a, Fractional b) => a -> b
eularExpansion k =
  let dseq i
        | mod i 3 == 2 = (fromIntegral (i + 1) * 2) / 3
        | otherwise    = 1.0
  in 2 + contFrac (\x -> 1.0) dseq k

--
-- TEST
--
-- *Main> eularExpansion 5
-- 2.71875
-- *Main> eularExpansion 10
-- 2.7182817182817183
-- *Main> eularExpansion 50
-- 2.7182818284590455
-- *Main> eularExpansion 100
-- 2.7182818284590455
-- *Main> eularExpansion 1500
-- 2.7182818284590455
--
--



