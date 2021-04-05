-- The solution of exercise 1.30
-- The sum procedure above generates a linear recursion. The procedure
-- can be rewritten so that the sum is performed iteratively. Show how
-- to do this process.
--
-- -------- (above from SICP)
--

-- Compute cube of a number
cube :: Num a => a -> a
cube x = x ^ 3

-- New abstrack summation procedure (iterative process)
sigmaSum :: (Num a, Ord b) => (b -> a) -> b -> (b -> b) -> b -> a
sigmaSum term a next b =
  let iter m result =
        if m > b
        then result
        else iter (next m) (result + term m)
  in iter a 0

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
-- *Main> simpson cube 0 1 1000
-- 0.25
-- *Main> integral cube 0 1 0.001
-- 0.24999987500000073
-- *Main> simpson cube 0 1 100
-- 0.25
-- *Main> integral cube 0 1 0.01
-- 0.24998750000000042
--



