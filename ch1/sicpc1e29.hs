-- The solution of exercise 1.29
-- Simpson's Rule is a more accurate method of numerical integration than
-- the method illustrated above. Using Simpson's Rule, the integral of a
-- function f between a and b is approximated as
--
--     (h / 3) * [y_0 + 4 * y_1 + 2 * y_2
--                    + 4 * y_3 + 2 * y_4
--                    + 4 * y_5 + 2 * y_6
--                    +   ...   +   ...
--                    + 4 * y_{n - 2} + 2 * y_{n - 1} + y_n ]
--
-- where h = (b - a) / n, for some even integer n, and y_k = f(a + k * h).
-- (Increasing n increases the accuracy of the approximation.) Define a
-- procedure that takes as arguments f, a, b, and n and returns the value
-- of the integral, computed using Simpson's Rule. Use your procedure to
-- integrate `cube` between 0 and 1 (with n = 100 and n = 1000), and
-- compare the results to those of the `integral` procedure shown above.
--
-- -------- (above from SICP)
--

-- Compute cube of a number
cube :: Num a => a -> a
cube x = x ^ 3

-- We write a procedure that expresses the concept of summation itself
-- rather than only procedures that compute particular sums.
sigmaSum :: (Num a, Ord b) => (b -> a) -> b -> (b -> b) -> b -> a
sigmaSum term a next b =
  if a > b
  then 0
  else term a + sigmaSum term (next a) next b

-- The simpson integral procedure
simpson :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> a
simpson f a b n =
  let h = (b - a) / n
      iter k = f (a + k * h)
      next m = m + 2
  in (h / 3) * ((f a) + (f b) +
               4 * sigmaSum iter 1 next (n - 1) +
               2 * sigmaSum iter 2 next (n - 2))

-- The integral computation procedure above
integral :: (Fractional a, Ord a) => (a -> a) -> a -> a -> a -> a
integral f a b dx =
  dx * sigmaSum f (a + dx / 2) (\x -> x + dx) b

--
-- TEST
-- 
-- *Main> integral cube 0 1 0.01
-- 0.24998750000000042
-- *Main> integral cube 0 1 0.001
-- 0.249999875000001
-- *Main> simpson cube 0 1 100
-- 0.25000000000000006
--



