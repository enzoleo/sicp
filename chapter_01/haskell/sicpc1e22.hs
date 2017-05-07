-- The solution of exercise 1.22
-- Most Lisp implementations include a primitive called `runtime` that
-- returns an integer that specifies the amount of time the system has been
-- running (measured, for example, in microseconds).
--
-- Write a procedure `search-for-primes` that checks the primality of
-- consecutive odd integers in a specified range. Use your procedure to
-- find the three smallest primes larger than 1000; larger than 10,000;
-- larger than 100,000; larger than 1,000,000. Note the time needed to test
-- each prime.
--
-- Since the testing algorithm has order of growth of O(sqrt(n)), you
-- should expect that testing for primes around 10,000 should take about
-- sqrt(10) times as long as testing for primes around 1000. Do your timing
-- data bear this out? How well do the data for 100,000 and 1,000,000
-- support the sqrt(n) prediction? Is your result compatible with the
-- notion that programs on your machine run in time proportional to the
-- number of steps required for the computation?
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
    let findDivisor m td
          | td * td > m   = m
          | mod m td == 0 = td
          | otherwise     = findDivisor m (td + 1)
    in findDivisor n 2

-- Test a number is a prime or not
isPrime n
  | n < 0     = isPrime (-n)
  | n == 0    = False
  | n == 1    = False
  | otherwise = smallestDivisor n == n
        
--
-- This procedure search all odd primes between lower and upper and display
-- all of them onto the screen.
--
searchPrimes :: (Integral a, Show a) => a -> a -> IO ()
searchPrimes lower upper =
  let checkPrime n =
        if isPrime n
        then putStrLn ("prime: " ++ show n)
        else return ()
      testOddPrimes counter maxCount =
        if counter > maxCount
        then return ()
        else do checkPrime counter
                testOddPrimes (counter + 2) maxCount
  in case (mod lower 2)
     of 1 -> testOddPrimes lower upper
        0 -> testOddPrimes (lower + 1) upper

--
-- This procedure search all odd primes between lower and upper and store
-- all of them into a vector.
--
-- Attention: We save all primes into a vector, so a big space is possible
-- to be needed if the difference between @upper and @lower is too big.
-- Use it carefully!
--
-- Test an example: we use this procedure to find all odd prime numbers
-- between 2175 and 3986:
--
-- *Main> searchPrimesVec 2175 3986
-- [2179,2203,2207,2213,2221,2237,2239,2243,2251,2267,2269,2273,2281,2287,
--  2293,2297,2309,2311,2333,2339,2341,2347,2351,2357,2371,2377,2381,2383,
--  2389,2393,2399,2411,2417,2423,2437,2441,2447,2459,2467,2473,2477,2503,
--  2521,2531,2539,2543,2549,2551,2557,2579,2591,2593,2609,2617,2621,2633,
--  2647,2657,2659,2663,2671,2677,2683,2687,2689,2693,2699,2707,2711,2713,
--  2719,2729,2731,2741,2749,2753,2767,2777,2789,2791,2797,2801,2803,2819,
--  2833,2837,2843,2851,2857,2861,2879,2887,2897,2903,2909,2917,2927,2939,
--  2953,2957,2963,2969,2971,2999,3001,3011,3019,3023,3037,3041,3049,3061,
--  3067,3079,3083,3089,3109,3119,3121,3137,3163,3167,3169,3181,3187,3191,
--  3203,3209,3217,3221,3229,3251,3253,3257,3259,3271,3299,3301,3307,3313,
--  3319,3323,3329,3331,3343,3347,3359,3361,3371,3373,3389,3391,3407,3413,
--  3433,3449,3457,3461,3463,3467,3469,3491,3499,3511,3517,3527,3529,3533,
--  3539,3541,3547,3557,3559,3571,3581,3583,3593,3607,3613,3617,3623,3631,
--  3637,3643,3659,3671,3673,3677,3691,3697,3701,3709,3719,3727,3733,3739,
--  3761,3767,3769,3779,3793,3797,3803,3821,3823,3833,3847,3851,3853,3863,
--  3877,3881,3889,3907,3911,3917,3919,3923,3929,3931,3943,3947,3967]
--
-- Use `V.length` to compute the number of odd primes between 2175
-- and 3986: 223 odd primes total:
--
-- *Main> V.length $ searchPrimesVec 2175 3986
-- 223
--

searchPrimesVec :: (Integral a) => a -> a -> V.Vector a
searchPrimesVec lower upper =
  let checkPrime n vector =
        if isPrime n
        then V.snoc vector n
        else vector
      testOddPrimes counter maxCount vector =
        if counter > maxCount
        then vector
        else testOddPrimes (counter + 2)
                           maxCount
                           (checkPrime counter vector)
      v0 = V.fromList []
  in case (mod lower 2)
     of 1 -> testOddPrimes lower upper v0
        0 -> testOddPrimes (lower + 1) upper v0

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
        then do let newPrime = nextPrime init
                searchPrimeCount newPrime (counter + 1) maxCount
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
-- 1.078655s
-- *Main> computeRuntime 1000000000000 3
-- 1000000000063
-- 3.2307s
-- *Main> computeRuntime 10000000000000 3
-- 10000000000099
-- 12.648188s
--



