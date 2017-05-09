-- The solution of exercise 1.24
-- Modify the `timed-prime-test` procedure of exercise 1.22 to use
-- `fast-prime?` (the Fermat method), and test each of the 12 primes you
-- found in that exercise. Since the Fermat test has O(log n) growth, how
-- would you expect the time to test primes near 1,000,000 to compare with
-- the time needed to test primes near 1000? Do your data bear this out?
-- Can you explain any discrepancy you find?
--
-- -------- (above from SICP)
--
import Data.Time
import System.Random
-- Run 'cabal install vector' first!
import qualified Data.Vector as V

-- Square function
square x = x * x

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

-- To implement the Fermat test, we need a procedure that computes the
-- exponential of a number modulo another number
expmod :: (Integral a) => a -> a -> a -> a
expmod base 0 m = 1
expmod base exp m =
  case (mod exp 2)
  of 0 -> mod (square $ expmod base (div exp 2) m) m
     1 -> mod (base * expmod base (exp - 1) m) m

-- Test a number is a prime or not
isPrime n
  | n < 0     = isPrime (-n)
  | n == 0    = False
  | n == 1    = False
  | otherwise = smallestDivisor n == n

-- This procedure points out whether the number is a carmichael number
-- If number @n is a prime, it is not a carmichael number. But if @n is not
-- a prime, use `carmichael-iter` procedure to check all numbers smaller
-- than @n whether they satisfy the expression: n | a ^ n - a. If all of
-- them satisfy this expression, the number @n is a carmichael number.
--
-- An available version:
--
--     isCarmichael n
--       | isPrime n = False
--       | otherwise =
--           let iter n 0 = True
--               iter n counter
--                 | expmod counter n n == counter = iter n (counter - 1)
--                 | otherwise                     = False
--           in iter n (n - 1)
--
-- But it is too slow. Here we use Korselt's criterion:
--
-- Wikipedia - Carmichael number:
-- [https://en.wikipedia.org/wiki/Carmichael_number]
--
-- Theorem (A. Korselt 1899):
-- A positive composite integer n is a Carmichael number if and only if n
-- is square-free, and for all prime divisors p of n, it is true that
--  p - 1 | n - 1.
--
-- It follows from this theorem that all Carmichael numbers are odd, since
-- any even composite number that is square-free (and hence has only one
-- prime factor of two) will have at least one odd prime factor, and thus
-- p - 1 | n - 1 results in an even dividing an odd, a contradiction.
--
isCarmichael :: (Integral a) => a -> Bool
isCarmichael 1 = False
isCarmichael n =
  let iter m 1 oldSd = True
      iter m n oldSd =
        let sd = smallestDivisor n
        in if sd == oldSd || mod (m - 1) (sd - 1) /= 0
           then False
           else let next = div n sd
                in iter m next sd
  in iter n n n

-- This procedure search all Carmichael numbers between lower and upper
-- and display all of them onto the screen. Notice that all Carmichael
-- numbers are odd due to Korselt's criterion.
searchCarmichaels :: (Integral a, Show a) => a -> a -> IO ()
searchCarmichaels lower upper =
  let checkCarmichael n =
        if isCarmichael n
        then putStrLn ("carmichael: " ++ show n)
        else return ()
      testCarmichaels counter maxCount =
        if counter > maxCount
        then return ()
        else do checkCarmichael counter
                testCarmichaels (counter + 2) maxCount
  in case (mod lower 2)
     of 0 -> testCarmichaels (lower + 1) upper
        1 -> testCarmichaels lower upper

--
-- This procedure search all Carmichael numbers between lower and upper
-- and store all of them into a vector.
--
-- Test an example: we use this procedure to find all Carmichael numbers
-- between 5000 and 20000:
--
-- *Main> searchCarmichaelsVec 5000 20000
-- [6601,8911,10585,15841]
--
--
searchCarmichaelsVec :: (Integral a) => a -> a -> V.Vector a
searchCarmichaelsVec lower upper =
  let checkCarmichael n vector =
        if isCarmichael n
        then V.snoc vector n
        else vector
      testCarmichaels counter maxCount vector =
        if counter > maxCount
        then vector
        else testCarmichaels (counter + 2)
                             maxCount
                             (checkCarmichael counter vector)
      v0 = V.fromList []
  in case (mod lower 2)
     of 0 -> testCarmichaels (lower + 1) upper v0
        1 -> testCarmichaels lower upper v0

-- Find the smallest Carmichael that is larger than n
nextCarmichael :: (Integral a) => a -> a
nextCarmichael n =
  let findCarmichael counter =
        if isCarmichael counter
        then counter
        else findCarmichael (counter + 1)
  in findCarmichael (n + 1)

-- Find the smallest m Carmichael numbers that are larger than n
nextCarmichaels :: (Integral a, Show a) => a -> a -> IO ()
nextCarmichaels n m =
  let searchCmCount init counter maxCount =
        if counter < maxCount
        then do let newCarmichael = nextCarmichael init
                putStrLn ("prime[" ++ show counter ++ "] "
                          ++ show newCarmichael)
                searchCmCount newCarmichael (counter + 1) maxCount
        else return ()
  in searchCmCount n 0 m

-- Find the m th Carmichael number that are larger than n
nextCarmichaelsNum :: (Integral a) => a -> a -> a
nextCarmichaelsNum n m =
  let searchCmCount init counter maxCount =
        if counter < maxCount
        then let newCarmichael = nextCarmichael init
             in searchCmCount newCarmichael (counter + 1) maxCount
        else init
  in searchCmCount n 0 m

--
-- Find the smallest m Carmichael numbers that are bigger than n and store
-- all of them in a vector. Test an example: find the next 30 Carmichael
-- numbers of 19999.
--
-- *Main> nextCarmichaelsVec 19999 30
-- [29341,41041,46657,52633,62745,63973,75361,101101,115921,126217,162401,
--  172081,188461,252601,278545,294409,314821,334153,340561,399001,410041,
--  449065,488881,512461,530881,552721,656601,658801,670033,748657]
--
nextCarmichaelsVec :: (Integral a) => a -> a -> V.Vector a
nextCarmichaelsVec n m =
  let searchCmCount init counter maxCount vector =
        if counter < maxCount
        then let newCarmichael = nextCarmichael init
             in searchCmCount newCarmichael
                              (counter + 1)
                              maxCount
                              (V.snoc vector newCarmichael)
        else vector
      v0 = V.fromList []
  in searchCmCount n 0 m v0

-- Compute the runtime of `nextCarmichaelsNum`
computeRuntime n m = do
  start <- getCurrentTime
  print $ nextCarmichaelsNum n m
  stop <- getCurrentTime
  print $ diffUTCTime stop start

--
-- *Main> computeRuntime 1 10
-- 29341
-- 1.11374s
-- *Main> computeRuntime 1 20
-- 162401
-- 10.418611s
-- *Main> computeRuntime 1 30
-- 410041
-- 36.649867s
-- *Main> computeRuntime 1 40
-- 825265
-- 93.00673s
--



