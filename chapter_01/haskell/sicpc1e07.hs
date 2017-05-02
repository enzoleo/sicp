-- The solution of exercise 1.7
-- Square roots by Newton's method
-- The `good-enough?` test in exercise 1.6 will not be very effective for
-- finding the square roots of very small numbers (because the number used
-- for comparison is always a positive number which means there always
-- exists smaller numbers). Also, in real computers, arithmetic operations
-- are almost always performed with limited precision. This makes our test
-- inadequate for very large numbers.
--
-- An alternative strategy for implementing `good-enough?` is to watch how
-- `guess` changes from one iteration to the next and to stop when the
-- change is a very small fraction of the guess. here we simply design a
-- square-root procedure that uses this kind of end test.
-- 

average x y = (x + y) / 2.0

-- Improve guess
-- A guess is improved by averaging it with the quotient of the radicand
-- and the old guess.
improve guess x = average guess (x * 1.0 / guess)

-- Good enough?
goodEnough newGuess oldGuess =
  abs ((newGuess - oldGuess) * 1.0 / oldGuess) < 0.0001

-- Compute the square root of x with initial value.
-- Newton method is used here.
sqrt_iter :: Float -> Float -> Float
sqrt_iter guess x =
  if x < 0
  then error "negative number has no real square root."
  else if x == 0
       then 0
       else let newGuess = improve guess x
            in if goodEnough newGuess guess
               then newGuess
               else sqrt_iter (improve newGuess x) x



