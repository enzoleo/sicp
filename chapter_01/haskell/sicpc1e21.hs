-- The solution of exercise 1.21
-- Use the `smallest-divisor` procedure to find the smallest divisor of
-- each of the following numbers: 199, 1999, 19999
--
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

--
-- Test: find the smallest divisor of 199, 1999, 19999.
--
-- *Main> smallestDivisor 199
-- 199
-- *Main> smallestDivisor 1999
-- 1999
-- *Main> smallestDivisor 19999
-- 7
--


