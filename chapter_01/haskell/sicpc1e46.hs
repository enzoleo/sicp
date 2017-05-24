-- The solution of exercise 1.46
-- Several of the numerical methods described in this chapter are instances
-- of an extremely general computational strategy known as `iterative
-- improvement`. Iterative improvement says that, to compute something,
-- we start with an initial guess for the answer, test if the guess is good
-- enough, and otherwise improve the guess and continue the process using
-- the improved guess as the new guess. Write a procedure `iterative-
-- improve` that takes two procedures as arguments: a method for telling
-- whether a guess is good enough and a method for improving a guess.
-- `Iterative-improve` should return as its value a procedure that takes a
-- guess as argument and keeps improving the guess until it is good enough.
-- Rewrite the `sqrt` procedure of section 1.1.7 and the `fixed-point`
-- procedure of section 1.3.3 in terms of `iterative-improve`.
--
-- -------- (above from SICP)
--

-- The abstract iterative-improve procedure
iterImprove :: (a -> a -> Bool) -> (a -> a) -> a -> a
iterImprove closeEnough improve =
  let try guess =
        if closeEnough guess next then next else try next
        where next = improve guess
  in \firstGuess -> try firstGuess

-- The tolerance used in the fixed-point procedure
tolerance = 0.00001

-- Fix-point computation procedure
fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess =
  let closeEnough v1 v2 =
        abs (v1 - v2) < tolerance
  in iterImprove closeEnough f firstGuess

-- The new `sqrt` procedure
newSqrt :: (Fractional a, Ord a) => a -> a
newSqrt x =
  let closeEnough newGuess oldGuess =
        abs (newGuess - oldGuess) / oldGuess < 0.0001
      improve guess = (guess + x / guess) / 2
  in iterImprove closeEnough improve 1.0

--
-- TEST
--
-- *Main> fixedPoint cos 1.0
-- 0.7390822985224024
-- *Main> newSqrt 576
-- 24.000000025758766
--



