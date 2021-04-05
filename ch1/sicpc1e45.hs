-- The solution of exercise 1.45
-- We saw in section 1.3.3 that attempting to compute square roots by
-- naively finding a fixed point of y -> x/y does not converge, and that
-- this can be fixed by average damping. The same method works for finding
-- cube roots as fixed points of the average-damped y -> x / y ^ 2.
--
-- Unfortunately, the process does not work for fourth roots -- a single
-- average damp is not enough to make a fixed-point search for y ->
-- x / y ^ 3 converge. On the other hand, if we average damp twice (i.e.,
-- use the average damp of the average damp of y -> x / y ^ 3) the fixed-
-- point search does (i.e., use the average damp of the average damp of y
-- converge. Do some experiments to determine how many average damps are
-- required to compute nth roots as a fixed-point search based upon
-- repeated average damping of y -> x / y ^ {n-1} . Use this to implement
-- a simple procedure for computing nth roots using fixed-point, average-
-- damp, and the repeated procedure of exercise 1.43. Assume that any
-- arithmetic operations you need are available as primitives.
--
-- -------- (above from SICP)
--
import Debug.Trace

-- The tolerance used in the fixed-point procedure
tolerance = 0.00001

-- Fix-point computation procedure
-- Fix-point computation procedure
fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess =
  let closeEnough v1 v2 =
        abs (v1 - v2) < tolerance
      try guess step =
        let next = f guess
        in trace ("Step[" ++ show step ++ "] " ++ show next) $
           if closeEnough guess next
           then next
           else try next (step + 1)
  in try firstGuess 1

-- Define average dumping of a function
averageDamp :: Fractional a => (a -> a) -> a -> a
averageDamp f =
  \x -> (x + f x) / 2

-- Repeated calling a function
repeated :: (Eq a, Num a) => (b -> b) -> a -> b -> b
repeated f n =
  let repf x
        | n == 0    = x
        | n == 1    = f x
        | otherwise = (repeated f (n - 1)) $ f x
  in repf

-- Compute the nth root of positive number a
-- Two available computation algorithms are given here
nthRoot :: Integral a => Double -> a -> Double
nthRoot a n =
  let num = fromIntegral n
      func x =
        (a / (x ^ (n - 1)) + (num * x - x)) / num
  in fixedPoint func 1.0
  
-- This `sqlg` procedure computes [log2 n]
-- Use this procedure to get the exact times of `average-damping`
-- This procedure is used to do comparison
nthRootComp :: Integral a => Double -> a -> Double
nthRootComp a n =
  let sqlg m
        | m > 2 = 1 + sqlg (div m 2)
        | m < 2 = 0
        | otherwise = 1
      p = sqlg n
      fun x = a / (x ^ (n - 1))
  in fixedPoint (repeated averageDamp p fun) 1.0

--
-- There exists an important question: how many average-damping do we need
-- to make sure the newton sequence is convergent?
--
-- Assumpt that we intend to compute the k th root of a positive real
-- number a (note it q = a ^ (1 / k)) and we use newton-method p times of
-- average-damping, which means:
--
--
--     x_{n + 1} = (1 - 1 / 2 ^ p) * x_n + a / ((2 ^ p) * x_n ^ (k - 1))
--     x_0 = 1.0
--
-- From this formula, we get that:
--
--     x_{n + 1} - q          1
--    --------------- = 1 - ----- * (1 + r + r ^ 2 + ... + r ^ (k - 1))
--        x_n - q           2 ^ p
--
--    r = q / x_n
--
-- Here we prove p = [log_2 k] is available.
-- If x_n < q, then r > 1 and
--
--     x_{n + 1} - q          1
--    --------------- < 1 - ----- * k <= 0
--        x_n - q           2 ^ p
--
-- So we konw that x_{n + 1} > q. Thus we can only consider the other
-- situation where x_n > q (r < 1):
--
-- From k <= 2 ^ {p + 1} - 1 we know that:
--
--     x_{n + 1} - q          1              2 ^ {p + 1} - 1          1
--    --------------- > 1 - ----- * k >= 1 - --------------- = -1 + -----
--        x_n - q           2 ^ p                 2 ^ p             2 ^ p
--
--     x_{n + 1} - q          1     r + ... + r ^ (k - 1)         1
--    --------------- = 1 - ----- - --------------------- < 1 - -----
--        x_n - q           2 ^ p          2 ^ p                2 ^ p
--
-- From the two inequalities above we know:
--
--    |  x_{n + 1} - q  |         1
--    | --------------- | < 1 - -----
--    |     x_n - q     |       2 ^ p
--
-- Then we know the newton method must be convergent.
--

-- TEST
--
-- =======================================================================
-- *Main> nthRootComp 16384 7
-- Step[1] 4096.75
-- Step[2] 3072.5625
-- Step[3] 2304.421875
-- Step[4] 1728.31640625
-- Step[5] 1296.2373046875
-- ...
-- Step[60] 3.99999261986268
-- Step[61] 4.000005535174477
-- Step[62] 3.9999958486593545
-- 3.9999958486593545
-- =======================================================================
-- *Main> nthRootComp 167854676913 1879
-- ...
-- Step[19358] 1.0138666506591083
-- Step[19359] 1.0138373173834636
-- Step[19360] 1.013861660760957
-- Step[19361] 1.013841253818456
-- Step[19362] 1.013858223403032
-- Step[19363] 1.0138440136529752
-- Step[19364] 1.0138558454409683
-- Step[19365] 1.0138459460537739
-- 1.0138459460537739
-- =======================================================================
-- *Main> nthRoot 16384 7
-- Step[1] 2341.4285714285716
-- Step[2] 2006.9387755102039
-- Step[3] 1720.2332361516035
-- Step[4] 1474.4856309870886
-- Step[5] 1263.8448265603618
-- ...
-- Step[45] 4.000059123541009
-- Step[46] 4.000000002621592
-- Step[47] 4.000000000000001
-- 4.000000000000001
-- =======================================================================
-- *Main> nthRoot 167854676913 1879
-- ...
-- Step[34363] 1.0157466082369992
-- Step[34364] 1.01522217581812
-- Step[34365] 1.014724462848865
-- Step[34366] 1.0142913876426143
-- Step[34367] 1.0139900269863353
-- Step[34368] 1.0138670120269806
-- Step[34369] 1.0138506757001784
-- Step[34370] 1.0138504234868493
-- 1.0138504234868493
--



