-- The solution of exercise 1.16
-- Design a procedure that evolves an iterative exponential process that
-- uses successive squaring and uses a logarithmic number of steps, as does
-- `fast-expt`.
--
-- [Hint] Using the observation that (b ^ (n / 2)) ^ 2 = (b ^ 2) ^ (n / 2),
-- keep, along with the exponent n and the base b, an additional state
-- variable a, and define the state transformation in such a way that the
-- product a * b ^ n is unchanged from state to state. At the beginning of
-- the process a is taken to be 1, and the answer is given by the value of
-- a at the end of the process. In general, the technique of defining an
-- invariant quantity that remains unchanged from state to state is a
-- powerful way to think about the design of iterative algorithms.
--
-- -------- (above from SICP)
--

import Debug.Trace

fastExpt :: (Floating a, Integral b, Show a, Show b) => a -> b -> a
fastExpt x n =
  if n < 0
  then 1 / fastExpt x (-n)
  else
    let iter a base 0 = a
        iter a base n =
          trace ("iteration: " ++ show a ++ "  " ++
                  show base ++ "  " ++ show n) $
          case (mod n 2) of 1 -> iter (a * base) base (n - 1)
                            0 -> iter a (base * base) (n `div` 2)
    in iter 1 x n

--
-- Use trace to see the calls. For example, trace (fast-expt-iter 3 37):
--
-- *Main> fastExpt 3 37
-- iteration: 1.0  3.0  37
-- iteration: 3.0  3.0  36
-- iteration: 3.0  9.0  18
-- iteration: 3.0  81.0  9
-- iteration: 243.0  81.0  8
-- iteration: 243.0  6561.0  4
-- iteration: 243.0  4.3046721e7  2
-- iteration: 243.0  1.853020188851841e15  1
-- 4.502839058909974e17
--


