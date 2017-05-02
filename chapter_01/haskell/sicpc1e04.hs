-- The solution of exercise 1.4
-- Observe that our model of evaluation allows for combinations whose
-- operators are compound expressions. Use this observation to describe
-- the behavior of the following procedure.

-- This function returns a prefix operator. 
op x =
  if x > 0
  then (+)
  else (-)

plusAbs a b = (op b) a b

-- Note how the function works.
-- The expression above is equivalent to the math equation: a + |b|.
-- (op x) returns prefix operator (+) if b > 0 is true.


