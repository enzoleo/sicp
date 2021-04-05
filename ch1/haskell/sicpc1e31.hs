-- The solution of exercise 1.31
-- [a] The `sum` procedure is only the simplest of a vast number of similar
--     abstractions that can be captured as higher-order procedures. Write
--     an analogous procedure called `product` that returns the product of
--     the values of a function at points over a given range. Show how to
--     define factorial in terms of product. Also use `product` to compute
--     approximations to using the Wallis formula:
--
--     PI     2 * 4 * 4 * 6 * 6 * 8 * 8 * 10 * ...
--    ---- = --------------------------------------
--     4      3 * 3 * 5 * 5 * 7 * 7 * 9 * 9  * ...
--
-- [b] If your product procedure generates a recursive process, write one
--     that generates an iterative process. If it generates an iterative
--     process, write one that generates a recursive process.
--
-- -------- (above from SICP)
--
import Data.Time

-- The square procedure
square :: Num a => a -> a
square x = x * x

-- New abstract product procedure (iterative process)
piProd :: (Num a, Ord b) => (b -> a) -> b -> (b -> b) -> b -> a
piProd term a next b =
  let iter m result
        | m > b     = result
        | otherwise = iter (next m) (result * term m)
  in iter a 1

-- New abstract product procedure (recursive process)
piProdRec :: (Num a, Ord b) => (b -> a) -> b -> (b -> b) -> b -> a
piProdRec term a next b
  | a > b     = 1
  | otherwise = term a * piProdRec term (next a) next b

-- The procedure to compute the value of PI approximately using the famous
-- John-Wallis formula. (This is an iterative process)
wallisPi :: (Fractional a, Ord a) => a -> a
wallisPi n =
  let term m = 1 - 1.0 / (square m)
  in 4 * piProd term 3 (\x -> x + 2) (2 * n + 1)

-- The procedure to compute the value of PI approximately using the famous
-- John-Wallis formula. (This is an recursive process)
wallisPiRec :: (Fractional a, Ord a) => a -> a
wallisPiRec n =
  let term m = 1 - 1.0 / (square m)
  in 4 * piProdRec term 3 (\x -> x + 2) (2 * n + 1)

-- Compute the runtime of @wallisPi and @wallisPiRec
computeRuntime n = do
  start1 <- getCurrentTime
  print $ wallisPi n
  stop1 <- getCurrentTime
  print $ diffUTCTime stop1 start1
  start2 <- getCurrentTime
  print $ wallisPiRec n
  stop2 <- getCurrentTime
  print $ diffUTCTime stop2 start2

--
-- TEST
--
-- The runtime test result is interesting. Recursive process costs less
-- time than iterative process (about 10%). It is easy to explain why such
-- case occurs: the interpreter uses normal-order evaluation (Haskell uses
-- lazy evaluation).
--
-- *Main> computeRuntime 1000000
-- 3.1415934389872975
-- 2.170803s
-- 3.141593438981073
-- 2.000254s
-- *Main> computeRuntime 10000000
-- 3.1415927321451758
-- 22.222008s
-- 3.1415927321166954
-- 20.114472s
--



