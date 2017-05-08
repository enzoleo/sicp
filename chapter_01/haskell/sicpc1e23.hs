-- The solution of exercise 1.23
-- The `smallest-divisor` procedure shown at the start of this section does
-- lots of needless testing: After it checks to see if the number is
-- divisible by 2 there is no point in checking to see if it is divisible
-- by any larger even numbers. This suggests that the values used for
-- `test-divisor` should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7,
-- 9,... . To implement this change, define a procedure `next` that returns
-- 3 if its input is equal to 2 and otherwise returns its input plus 2.
-- Modify the `smallest-divisor` procedure to use (next test-divisor)
-- instead of (+ test-divisor 1). With `timed-prime-test` incorporating
-- this modified version of `smallest-divisor`, run the test for each of
-- the 12 primes found in exercise 1.22. Since this modification halves the
-- number of test steps, you should expect it to run about twice as fast.
-- Is this expectation confirmed? If not, what is the observed ratio of the
-- speeds of the two algorithms, and how do you explain the fact that it is
-- different from 2?
--
-- -------- (above from SICP)
--
import Data.Time
-- Run 'cabal install vector' first!
import qualified Data.Vector as V

-- Find the smallest divisor of an integer
smallestDivisor :: (Integral a) => a -> a
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
isPrime n
  | n < 0     = isPrime (-n)
  | n == 0    = False
  | n == 1    = False
  | otherwise = smallestDivisor n == n

-- Find the smallest prime that is larger than n
nextPrime :: (Integral a) => a -> a
nextPrime n =
  let findPrime counter =
        if isPrime counter
        then counter
        else findPrime (counter + 1)
  in findPrime (n + 1)

-- Find the smallest m primes that are larger than n
nextPrimes :: (Integral a, Show a) => a -> a -> IO ()
nextPrimes n m =
  let searchPrimeCount init counter maxCount =
        if counter < maxCount
        then do let newPrime = nextPrime init
                putStrLn ("prime[" ++ show counter ++ "] "
                          ++ show newPrime)
                searchPrimeCount newPrime (counter + 1) maxCount
        else return ()
  in searchPrimeCount n 0 m

-- Find the m th primes that are larger than n
nextPrimesNum :: (Integral a) => a -> a -> a
nextPrimesNum n m =
  let searchPrimeCount init counter maxCount =
        if counter < maxCount
        then let newPrime = nextPrime init
             in searchPrimeCount newPrime (counter + 1) maxCount
        else init
  in searchPrimeCount n 0 m

--
-- Find the smallest m primes that are bigger than n and store all of them
-- in a vector. Test an example: find the next 100 odd primes of 19999.
--
-- *Main> nextPrimesVec 19999 100
-- [20011,20021,20023,20029,20047,20051,20063,20071,20089,20101,20107,
--  20113,20117,20123,20129,20143,20147,20149,20161,20173,20177,20183,
--  20201,20219,20231,20233,20249,20261,20269,20287,20297,20323,20327,
--  20333,20341,20347,20353,20357,20359,20369,20389,20393,20399,20407,
--  20411,20431,20441,20443,20477,20479,20483,20507,20509,20521,20533,
--  20543,20549,20551,20563,20593,20599,20611,20627,20639,20641,20663,
--  20681,20693,20707,20717,20719,20731,20743,20747,20749,20753,20759,
--  20771,20773,20789,20807,20809,20849,20857,20873,20879,20887,20897,
--  20899,20903,20921,20929,20939,20947,20959,20963,20981,20983,21001,
--  21011]
--
nextPrimesVec :: (Integral a) => a -> a -> V.Vector a
nextPrimesVec n m =
  let searchPrimeCount init counter maxCount vector =
        if counter < maxCount
        then let newPrime = nextPrime init
             in searchPrimeCount newPrime
                                 (counter + 1)
                                 maxCount
                                 (V.snoc vector newPrime)
        else vector
      v0 = V.fromList []
  in searchPrimeCount n 0 m v0

-- Compute the runtime of `nextPrimesNum`
computeRuntime n m = do
  start <- getCurrentTime
  print $ nextPrimesNum n m
  stop <- getCurrentTime
  print $ diffUTCTime stop start

--
-- *Main> computeRuntime 100000000000 3
-- 100000000057
-- 0.978514s
-- *Main> computeRuntime 1000000000000 3
-- 1000000000063
-- 2.987301s
-- *Main> computeRuntime 10000000000000 3
-- 10000000000099
-- 11.742541s
--



