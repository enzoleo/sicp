-- The solution of exercise 1.11
-- A function f is defined by the rule that f(n) = n if n < 3 and f(n) =
-- f(n - 1) + 2 * f(n - 2) + 3 * f(n - 3) if n >= 3. Write a procedure
-- that computes f by means of a recursive process. Write a procedure that
-- computes f by means of an iterative process
--

-- Recursive process
rec_f :: (Eq a, Integral a) => a -> a
rec_f n
  | n <= 0    = error "the input number must be positive."
  | n <= 3    = n
  | otherwise = rec_f (n - 1) +
                rec_f (n - 2) * 2 +
                rec_f (n - 3) * 3

-- Iterative process
itr_f :: (Eq a, Integral a) => a -> a
itr_f n =
  if n <= 0
  then error "the input number must be positive."
  else
    let itr_f_tmp a b c 1 = c
        itr_f_tmp a b c count =
          itr_f_tmp (a + (b * 2) + (c * 3)) a b (count - 1)
    in itr_f_tmp 3 2 1 n

-- The iterative process and the recursive process return the same result
-- in case the input parameter is integer. In other word, they return
-- different results when the input parameter @n is not an integer.


