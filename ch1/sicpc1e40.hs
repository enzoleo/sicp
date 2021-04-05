-- The solution of exercise 1.40
-- Define a procedure cubic that can be used together with the newtons-
-- method procedure in expressions of the form
--
--     (newtons-method (cubic a b c) 1)
--
-- to approximate zeros of the cubic x ^ 3 + a * x ^ 2 + b * x + c.
--
-- -------- (above from SICP)
--
import Debug.Trace

-- Define an infinitely small quantity
dx = 0.00001

-- The tolerance used in the fixed-point procedure
tolerance = 0.00001

-- Numerical differentiation
deriv :: (Double -> Double) -> Double -> Double
deriv f =
  \x -> (f (x + dx) - f x) / dx

-- Newton transformation
newtonTransform :: (Double -> Double) -> Double -> Double
newtonTransform f =
  \x -> x - f x / (deriv f) x 

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

-- Newton's method to search fixed point
newtonMethod :: (Double -> Double) -> Double -> Double
newtonMethod f guess =
  fixedPoint (newtonTransform f) guess

-- Define a cubic polynomial function
cubic :: Num a => a -> a -> a -> a -> a
cubic a b c =
  \x -> x * x * x + a * x * x + b * x + c

-- Root a cubic equation x ^ 3 + a * x ^ 2 + b * x + c = 0
cubicEqRoot :: Double -> Double -> Double -> Double
cubicEqRoot a b c =
  newtonMethod (cubic a b c) 1.0

--
-- Example: Root equation: x ^ 3 - 3 * x ^ 2 + 5 * x - 6 = 0
-- Input coefficients a, b and c: -3 5 -6
--
-- *Main> cubicEqRoot (-3) 5 (-6)
-- Step[1] 2.4999999999568665
-- Step[2] 2.1142876979345435
-- Step[3] 2.0073667843043377
-- Step[4] 2.000032477787667
-- Step[5] 2.0000000008277357
-- Step[6] 2.000000000000005
-- 2.000000000000005
--



