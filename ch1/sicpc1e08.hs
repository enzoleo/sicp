-- The solution of exercise 1.8
-- Newton's method for cube roots is based on the fact that if y is an
-- approximation to the cube root of x, then a better approximation is
-- given by the value (x / y ^ 2 + 2 * y) / 3.
--
-- You can prove that give an initial value 1.0, then the formula above
-- defines a sequence of approximate cube roots of x and it converges to
-- the cube roots of x. So we use this formula to implement a cube-root
-- procedure analogous to the square-root procedure.
-- 

-- Improve guess
-- A guess is improved by averaging it with the quotient of the radicand
-- and the old guess.
improve guess x = (guess * 2.0 + x * 1.0 / (guess * guess)) / 3

-- Good enough?
goodEnough newGuess oldGuess =
  abs ((newGuess - oldGuess) * 1.0 / oldGuess) < 0.0001

-- Compute the square root of x with initial value.
-- Newton method is used here.
curt_iter :: Float -> Float -> Float
curt_iter guess x =
  if x == 0
  then 0
  else let newGuess = improve guess x
       in if goodEnough newGuess guess
          then newGuess
          else curt_iter (improve newGuess x) x



