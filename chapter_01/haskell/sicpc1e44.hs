-- The solution of exercise 1.44
-- The idea of smoothing a function is an important concept in signal
-- processing. If f is a function and dx is some small number, then the
-- smoothed version of f is the function whose value at a point x is the
-- average of f(x - dx), f(x), and f(x + dx). Write a procedure smooth
-- that takes as input a procedure that computes f and returns a procedure
-- that computes the smoothed f. It is sometimes valuable to repeatedly
-- smooth a function (that is, smooth the smoothed function, and so on) to
-- obtained the n-fold smoothed function. Show how to generate the `n-fold
-- smoothed function of any given function using `smooth` and `repeated`
-- from exercise 1.43.
--
-- -------- (above from SICP)
--

-- Define an infinitely small quantity
dx = 0.00001

-- Necessary functions
selfPow :: Floating a => a -> a
selfPow x = exp (x * log x)

-- Repeated function
repeated :: (Eq a, Num a) => (b -> b) -> a -> b -> b
repeated f n =
  let repf x
        | n == 0    = x
        | n == 1    = f x
        | otherwise = (repeated f (n - 1)) $ f x
  in repf

-- Smooth a function
smooth :: Fractional a => (Double -> a) -> Double -> a
smooth f =
  \x -> (f x + f (x - dx) + f (x - dx)) / 3

-- Define a n fold smoothed function
nf_smooth
  :: (Eq a, Num a, Fractional b) =>
     (Double -> b) -> a -> Double -> b
nf_smooth f n =
  (repeated smooth n) f

--
-- TEST
-- *Main> (nf_smooth selfPow 5) 5
-- 3124.728196935879
--



