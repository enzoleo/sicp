-- The solution of exercise 1.39
-- A continued fraction representation of the tangent function was
-- published in 1770 by the German mathematician J.H. Lambert:
--
--                       x
--     tan x = ----------------------
--                        x ^ 2
--              1 - -----------------
--                           x ^ 2
--                   3 - ------------
--                             x ^ 2
--                        5 - -------
--                              ...
--
-- where x is in radians. Define a procedure `(tan-cf x k)` that computes
-- an approximation to the tangent function based on Lambert's formula.
-- `K` specifies the number of terms to compute, as in exercise 1.37.
--
-- -------- (above from SICP)
--

-- The `cont-frac` procedure as a recursive process
contFrac
  :: (Num a, Eq a, Fractional b) =>
     (a -> b) -> (a -> b) -> a -> b
contFrac n d k =
  let iter counter =
        n counter / (d counter +
                      if k == counter
                      then 0
                      else iter (counter + 1))
  in iter 1

-- The recursive process to rewrite the tangent function using Lambert's
-- formula (called `tan-cf` procedure)
tan_cf :: (Fractional a, Integral b) => a -> b -> a
tan_cf x k =
  let nseq i = x * x
      dseq i =
        if mod i 2 == 0 then tmp else (-tmp)
        where tmp = fromIntegral (i * 2 + 1) 
  in x / (1 + contFrac nseq dseq k)

--
-- TEST
--
-- *Main> tan_cf (pi/4) 100
-- 1.0
-- *Main> tan_cf (pi/3) 100
-- 1.732050807568877
-- *Main> tan_cf (pi/6) 100
-- 0.5773502691896257
-- *Main> tan_cf (pi/2) 100
-- Infinity
-- *Main> tan_cf pi 100
-- -1.4135798584282297e-16
--



