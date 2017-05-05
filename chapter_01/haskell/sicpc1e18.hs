-- The solution of exercise 1.18
-- Using the results of exercises 1.16 and 1.17, devise a procedure that
-- generates an iterative process for multiplying two integers in terms of
-- adding, doubling, and halving and uses a logarithmic number of steps.
--
-- -------- (above from SICP)
--
import Debug.Trace

-- Definition of simple procedures
-- Notice that `halve` only works on integers
double x = trace ("double " ++ show x) $ x * 2
halve x = trace ("halve " ++ show x) $ x `div` 2

mul_itr :: (Integral a, Show a) => a -> a -> a
mul_itr a b =
  if b < 0
  then mul_itr a (-b)
  else
    let iter ans x 0 = ans
        iter ans x y =
          trace ("iteration: ans = " ++ show ans ++ " \targs: " ++
                 show x ++ " " ++ show y) $
          case (mod y 2) of 0 -> iter ans (double x) (halve y)
                            1 -> iter (ans + x) x (y - 1)
    in iter 0 a b

--
-- Use trace to see the calls. For example, trace (multiply-itr 45 87):
--
-- *Main> mul_itr 45 87
-- iteration: ans = 0	        args: 45 87
-- iteration: ans = 45	        args: 45 86
-- halve 86
-- double 45
-- iteration: ans = 45	        args: 90 43
-- iteration: ans = 135	        args: 90 42
-- halve 42
-- double 90
-- iteration: ans = 135	        args: 180 21
-- iteration: ans = 315	        args: 180 20
-- halve 20
-- double 180
-- iteration: ans = 315	        args: 360 10
-- halve 10
-- double 360
-- iteration: ans = 315         args: 720 5
-- iteration: ans = 1035        args: 720 4
-- halve 4
-- double 720
-- iteration: ans = 1035        args: 1440 2
-- halve 2
-- double 1440
-- iteration: ans = 1035        args: 2880 1
-- 3915
--



