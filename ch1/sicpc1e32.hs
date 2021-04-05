-- The solution of exercise 1.32
-- [a] Show that sum and product (exercise 1.31) are both special cases of
--     a still more general notion called accumulate that combines a
--     collection of terms, using some general accumulation function:
--     (accumulate combiner null-value term a next b) Accumulate takes as
--     arguments the same term and range specifications as sum and product,
--     together with a combiner procedure (of two arguments) that specifies
--     how the current term is to be combined with the accumulation of the
--     preceding terms and a null-value that specifies what base value to
--     use when the terms run out. Write accumulate and show how sum and
--     product can both be defined as simple calls to accumulate.
--
-- [b] If your accumulate procedure generates a recursive process, write
--     one that generates an iterative process. If it generates an
--     iterative process, write one that generates a recursive process.
--
-- -------- (above from SICP)
--
import Data.Time

-- The square procedure
square :: Num a => a -> a
square x = x * x

-- The abstract accumulate procedure (iterative and recursive process)
-- The function type here is complicated.
accumulate :: Ord a =>
              (b -> r -> b) -> b -> (a -> r) -> a -> (a -> a) -> a -> b
accumulate combiner nullValue term a next b =
  let iter m result
        | m > b     = result
        | otherwise = iter (next m) (combiner result $ term m)
  in iter a nullValue

accumulate_rec :: Ord a =>
                  (b -> r -> b) -> b -> (a -> r) -> a -> (a -> a) -> a -> b
accumulate_rec combiner nullValue term a next b
  | a > b     = nullValue
  | otherwise = combiner (accumulate_rec combiner nullValue
                                         term (next a) next b) (term a)

-- New abstract sum procedure (iterative and recursive process)
-- Use accumulate procedure (combiner = "+" procedure)
sigmaSum :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sigmaSum term a next b =
  accumulate (+) 0 term a next b

sigmaSumRec :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
sigmaSumRec term a next b =
  accumulate_rec (+) 0 term a next b

-- New abstract product procedure (iterative and recursive process)
-- Use accumulate procedure (combiner = "*" procedure)
piProd :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
piProd term a next b =
  accumulate (*) 1 term a next b

piProdRec :: (Ord a, Num b) => (a -> b) -> a -> (a -> a) -> a -> b
piProdRec term a next b =
  accumulate_rec (*) 1 term a next b

wallisPi :: (Fractional a, Ord a) => a -> a
wallisPi n =
  let term m = 1 - 1.0 / (square m)
  in 4 * piProd term 3 (\x -> x + 2) (2 * n + 1)

wallisPiRec :: (Fractional a, Ord a) => a -> a
wallisPiRec n =
  let term m = 1 - 1.0 / (square m)
  in 4 * piProdRec term 3 (\x -> x + 2) (2 * n + 1)

-- Leibniz formula is used to test `sum`
leibnizPi :: (Fractional a, Ord a) => a -> a
leibnizPi n =
  8 * sigmaSum (\x -> 1 / (x * (x + 2))) 1 (\x -> x + 4) n

leibnizPiRec :: (Fractional a, Ord a) => a -> a
leibnizPiRec n =
  8 * sigmaSumRec (\x -> 1 / (x * (x + 2))) 1 (\x -> x + 4) n

-- Compute the runtime of @wallisPiRec and @leibnizPiRec
computeRuntime n = do
  start1 <- getCurrentTime
  print $ wallisPiRec n
  stop1 <- getCurrentTime
  print $ diffUTCTime stop1 start1
  start2 <- getCurrentTime
  print $ leibnizPiRec n
  stop2 <- getCurrentTime
  print $ diffUTCTime stop2 start2


--
-- TEST
--
-- *Main> computeRuntime 10000000
-- 3.1415927321166954
-- 27.646831s
-- 3.141592453589793
-- 4.109553s 
--



