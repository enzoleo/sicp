-- The solution of exercise 1.17
-- The exponentiation algorithms in this section are based on performing
-- exponentiation by means of repeated multiplication. In a similar way,
-- one can perform interger multiplication by means of repeated addition.
-- The following multiplication procedure (in which it is assumed that our
-- language can only add, not multiply) is analogous to the expt procedure:
--
--     (define (* a b)
--       (if (= b 0)
--           0
--           (+ a (* a (- b 1)))))
--
-- This algorithm takes a number of steps that is linear in b. Now suppose
-- we include, together with addition, operations `double`, which doubles
-- an integer, and `halve`, which divides an (even) integer by 2. Using
-- these, design a multiplication procedure analogous to `fast-expt` that
-- uses a logarithmic number of steps.
--
-- -------- (above from SICP)
--
import Debug.Trace

-- Definition of simple procedures
-- Notice that `halve` only works on integers
double x = trace ("double " ++ show x) $ x * 2
halve x = trace ("halve " ++ show x) $ x `div` 2

-- Multiply two integers (recursive process)
mul_rec :: (Integral a, Show a) => a -> a -> a
mul_rec a 0 = 0
mul_rec a b =
  if b < 0
  then mul_rec a (-b)
  else
    trace ("mul_rec " ++ show a ++ " " ++ show b) $
    case (mod b 2) of 0 -> double $ mul_rec a (halve b)
                      1 -> mul_rec a (b - 1) + a
--
-- Use trace to see the calls. For example, trace (multiply-rec 45 87):
--
-- *Main> mul_rec 45 87
-- mul_rec 45 87
-- mul_rec 45 86
-- halve 86
-- mul_rec 45 43
-- mul_rec 45 42
-- halve 42
-- mul_rec 45 21
-- mul_rec 45 20
-- halve 20
-- mul_rec 45 10
-- halve 10
-- mul_rec 45 5
-- mul_rec 45 4
-- halve 4
-- mul_rec 45 2
-- halve 2
-- mul_rec 45 1
-- double 45
-- double 90
-- double 225
-- double 450
-- double 945
-- double 1935
-- 3915
--



