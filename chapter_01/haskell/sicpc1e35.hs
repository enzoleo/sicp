-- The solution of exercise 1.35
-- Show that the golden ratio \phi (section 1.2.2) is a fixed point of the
-- transformation: x -> 1 + 1 / x, and use this fact to compute \phi by
-- means of the fixed-point procedure.
--
-- -------- (above from SICP)
--

-- The tolerance used in the fixed-point procedure
tolerance = 0.00001 :: Double

-- Fix-point computation procedure
fixedPoint :: (Double -> Double) -> Double -> Double
fixedPoint f firstGuess =
  let closeEnough v1 v2 =
        abs (v1 - v2) < tolerance
      try guess =
        let next = f guess
        in if closeEnough guess next
           then next
           else (try next)
  in try firstGuess

-- The procedure computes the golden ratio
golden_ratio :: Double -> Double
golden_ratio firstGuess =
  fixedPoint (\x -> 1 + 1 / x) firstGuess

--
-- TEST
--
-- *Main> golden_ratio 1.0
-- 1.6180327868852458
--



