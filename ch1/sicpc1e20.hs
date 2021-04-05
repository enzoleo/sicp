-- The solution of exercise 1.20
-- The process that a procedure generates is of course dependent on the
-- rules used by the interpreter. As an example, consider the iterative gcd
-- procedure.
import Debug.Trace

gcDivisor :: (Integral a, Show a) => a -> a -> a
gcDivisor a 0 = a
gcDivisor a b =
  trace ("gcd " ++ show a ++ " " ++ show b) $
  gcDivisor b (mod a b)


--
-- How many `modulo` operations are actually performed evaluation of
-- gcDivisor 206 40? Use trace to see the calls.
--
-- *Main> gc_divisor 206 40
-- gcd 206 40
-- gcd 40 6
-- gcd 6 4
-- gcd 4 2
-- 2
--



