-- The solution of exercise 1.28
-- One variant of the Fermat test that cannot be fooled is called the
-- Miller-Rabin test (Miller 1976; Rabin 1980). This starts from an
-- alternate form of Fermat's Little Theorem, which states that if n is a
-- prime number and a is any positive integer less than n, then a raised to
-- the (n - 1)st power is congruent to 1 modulo n. To test the primality of
-- a number n by the Miller-Rabin test, we pick a random number a < n and
-- raise a to the (n - 1)st power modulo n using the `expmod` procedure.
-- However, whenever we perform the squaring step in `expmod`, we check to
-- see if we have discovered a "nontrivial square root of 1 modulo n", that
-- is, a number not equal to 1 or n - 1 whose square is equal to 1 modulo
-- n. (This is why Miller-Rabin test cannot be fooled.)
--
-- Modify the `expmod` procedure to signal if it discovers a nontrivial
-- square root of 1, and use this to implement the Miller-Rabin test with a
-- procedure analogous to `fermat-test`. Check your procedure by testing
-- various known primes and non-primes.
--
-- HInt: One convenient way to make `expmod` signal is to have it return 0.
--
-- -------- (above from SICP)
--
import Data.Time
import System.Random
-- Run 'cabal install vector' first!
import qualified Data.Vector as V

-- Square function
square x = x * x

-- Check whether a number has nontrivial square root of 1
isNontrivialSqrt :: (Integral a) => a -> a -> Bool
isNontrivialSqrt 1 m = False
isNontrivialSqrt base m
  | base == m - 1 = False
  | otherwise     = mod (square base) m == 1

-- To implement Miller-Rabin test, we need a procedure that computes the
-- exponential of a number modulo another number
expmod :: (Integral a) => a -> a -> a -> a
expmod base 0 m = 1
expmod base exp m =
  if isNontrivialSqrt base m
  then 0
  else
    case (mod exp 2)
    of 0 -> mod (square $ expmod base (div exp 2) m) m
       1 -> mod (base * expmod base (exp - 1) m) m

-- Use Miller-Rabin test
-- Choose a random number between 1 and n - 1 using the procedure `random`,
-- which we assume is included as a primitive in Scheme. This is Miller-
-- Rabin test instead of Fermat test.
fastPrime :: (Integral a, Random a, RandomGen g) => a -> a -> g -> Bool
fastPrime n 0 initGen = True
fastPrime n times initGen =
  let (randNum, nextGen) = randomR (1, n - 1) initGen
  in if expmod randNum (n - 1) n == 1
     then fastPrime n (times - 1) nextGen
     else False

-- Test a number is a prime or not. Here we do fermat-test 20 times for
-- each number n. You can modify the number as you like.
isPrime n =
  fastPrime n 20 $ mkStdGen 0

-- Find the smallest prime that is larger than n
nextPrime :: (Integral a, Random a) => a -> a
nextPrime n =
  let findPrime counter =
        if isPrime counter
        then counter
        else findPrime (counter + 1)
  in findPrime (n + 1)

-- Find the smallest m primes that are larger than n
nextPrimes :: (Integral a, Show a, Random a) => a -> a -> IO ()
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
nextPrimesNum :: (Integral a, Random a) => a -> a -> a
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
nextPrimesVec :: (Integral a, Random a) => a -> a -> V.Vector a
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
-- *Main> computeRuntime 100000000000000 1000
-- 100000000033471
-- 16.338863s
-- *Main> computeRuntime 100000000000000000 1000
-- 100000000000039747
-- 22.023556s
-- *Main> computeRuntime 100000000000000000000 1000
-- 100000000000000048591
-- 32.159553s
--



