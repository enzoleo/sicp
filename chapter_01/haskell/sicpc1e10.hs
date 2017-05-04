-- The solution of exercise 1.10
-- The following procedure computes a mathematical function called
-- Ackermann's function.
--

-- Compute Ackermann numbers
ackermann :: (Eq a, Integral a) => a -> a -> a
ackermann x 0 = 0
ackermann x 1 = 2
ackermann 0 y = 2 * y
ackermann x y =
  ackermann (x - 1) (ackermann x (y - 1))

-- What are the values of the following expressions?
--
--     ackermann(1, 10)
--     ackermann(2, 4)
--     ackermann(3, 3)
--
-- Just compute them using the function defined above.
--

f n = ackermann 0 n
g n = ackermann 1 n
h n = ackermann 2 n

--
-- Give concise mathematical definitions for the functions computed by the
-- procedure f, g and h for positive integer values of n. For example,
-- the function k defined as following procedure:
--
--     k n = 5 * n * n
--
-- computes the mathematical formula 5n^2.
-- You can get deduce the mathematical formula the functions f, g and h
-- give merely from the definition of ackermann recursion:
--
--     f(n) = 2 * n
--     g(n) = 2 ^ n (n >= 1), g(0) = 0
--     h(n) = 2 ^ h(n - 1) (n >= 2), h(1) = 2, h(0) = 0
--
-- Compute some examples to check.
--


