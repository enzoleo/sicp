-- The solution of exercise 1.3
-- Define a procedure that takes three numbers as arguments and returns
-- the sum of the squares of the two larger numbers
--

-- Input three numbers from keyboard.
-- Return the sum of the squares of the two larger numbers.
glss a b c =
  if a > b
  then if b > c
       then a * a + b * b
       else a * a + c * c
  else if a > c
       then a * a + b * b
       else c * c + b * b
  

