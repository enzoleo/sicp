-- The solution of exercise 1.33
-- You can obtain an even more general version of `accumulate` (exercise
-- 1.32) by introducing the notion of a filter on the terms to be combined.
-- That is, combine only those terms derived from values in the range that
-- satisfy a specified condition. The resulting `filtered-accumulate`
-- abstraction takes the same arguments as accumulate, together with an
-- additional predicate of one argument that specifies the filter. Write
-- `filtered-accumulate` as a procedure. Show how to express the following
-- using `filtered-accumulate`:
--
-- [a] the sum of the squares of the prime numbers in the interval a to b
--     (assuming that you have a `prime?` predicate already written)
--
-- [b] the product of all the positive integers less than n that are
--     relatively prime to n (i.e., all positive integers i < n such that
--     GCD(i, n) = 1).
--
-- -------- (above from SICP)
--

-- The square procedure
square :: Num a => a -> a
square x = x * x

-- The abstract accumulate procedure (iterative process)
filteredAccumulate
  :: Ord a =>
     (a -> Bool) ->
     (b -> t -> b) -> b -> (a -> t) -> a -> (a -> a) -> a -> b
filteredAccumulate filter combiner nullValue term a next b =
  let iter m result
        | m > b     = result
        | otherwise = if filter m
                      then iter (next m) $ combiner result $ term m
                      else iter (next m) result
  in iter a nullValue

-- Find the smallest divisor of an integer
smallestDivisor :: Integral a => a -> a
smallestDivisor n =
  if n < 0
  then smallestDivisor (-n)
  else
    let next n
          | n == 2 = 3
          | n > 2  = n + 2
        findDivisor m td
          | td * td > m   = m
          | mod m td == 0 = td
          | otherwise     = findDivisor m (next td)
    in findDivisor n 2

-- Test a number is a prime or not
isPrime :: Integral a => a -> Bool
isPrime n
  | n < 0     = isPrime (-n)
  | n == 0    = False
  | n == 1    = False
  | otherwise = smallestDivisor n == n

-- New abstract filtered sum procedure (iterative process)
-- Use `filteredAccumulate` procedure (combiner = "+" procedure)
filteredSum
  :: (Ord a, Num b) =>
     (a -> Bool) -> (a -> b) -> a -> (a -> a) -> a -> b
filteredSum filter term a next b =
  filteredAccumulate filter (+) 0 term a next b

-- New abstract filtered product procedure (iterative process)
-- Use `filteredAccumulate` procedure (combiner = "*" procedure)
filteredProd
  :: (Ord a, Num b) =>
     (a -> Bool) -> (a -> b) -> a -> (a -> a) -> a -> b
filteredProd filter term a next b =
  filteredAccumulate filter (*) 1 term a next b

-- Compute the sum of the squares of the prime numbers in the interval a
-- to b (we already have `prime?` procedure)
sqPrimeSum :: Integral a => a -> a -> a
sqPrimeSum a b =
  filteredSum isPrime square a (\x -> x + 1) b

-- Compute the the product of all the positive integers less than n that
-- are relatively prime to n.
coPrimeProd :: Integral a => a -> a
coPrimeProd n =
  filteredProd (\x -> gcd n x == 1) id 1 (\x -> x + 1) n

--
-- TEST
-- We only define filteredAccumulate as iterative process.
--
-- *Main> sqPrimeSum 106 300
-- 1511819
-- *Main> coPrimeProd 30
-- 215656441
--



