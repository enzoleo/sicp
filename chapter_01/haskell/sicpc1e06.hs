-- The solution of exercise 1.6
-- Square roots by Newton's method
--
-- As a case in point, consider the problem of computing square roots.
-- We can define the square-root function as:
--     square(x) = the y, such that y >= 0 and y^2 = x
-- This describes a perfectly legitimate mathematical function. We could
-- use it to recognize whether one number is the square root of another,
-- or to describe a procedure. Indeed, it tells us almost nothing about
-- how to actually find the square root of a given number. It will not
-- help matters to rephrase this definition in pseudo-Lisp:
-- 
--     (define (sqrt x)
--       (the y (and (>= y 0)
--                   (= (square y) x))))
--
-- This only begs the question.
--
-- -------- (above from SICP)
--
-- How does one compute square roots? The most common way is to use
-- Newton's method of successive approximations.
--

average x y = (x + y) / 2.0

-- Improve guess
-- A guess is improved by averaging it with the quotient of the radicand
-- and the old guess.
improve guess x = average guess (x * 1.0 / guess)

-- It is important to point out what we mean by `good enough`. Here we
-- give a simple illustration (but it is not really a very good test).
-- The idea is to improve the answer until it is close enough so that its
-- square differs from the radicand by less than a predetermined tolerance
-- (here 0.0001 is set)
-- See exercise 1.7 to get a better good-enough? test.
goodEnough guess x = abs (guess * guess - x) < 0.0001

-- Compute the square root of x with initial value.
-- Newton method is used here.
sqrt_iter :: Float -> Float -> Float
sqrt_iter guess x =
  if x < 0
  then error "negative number has no real square root."
  else if x == 0
       then 0
       else if goodEnough guess x
            then guess
            else sqrt_iter (improve guess x) x



