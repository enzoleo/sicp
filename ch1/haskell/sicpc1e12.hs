-- The solution of exercise 1.12
-- The following pattern of numbers is called Pascal's triangle.
--
--                          1
--                        1   1
--                      1   2   1
--                    1   3   3   1
--                  1   4   6   4   1
--
-- The numbers at the edge of the triangle are all 1, and each number
-- inside the triangle is the sum of the two numbers above it. Write a
-- procedure that computes elements of Pascal's triangle by means of a
-- recursive process.
--

-- Run 'cabal install vector' first!
import qualified Data.Vector as V
               
-- Compute combinarotial number by means of a recursive process
recursive_pascal :: (Eq a, Integral a) => a -> a -> a
recursive_pascal m 0 = 1
recursive_pascal m n
  | m < 0 ||
    n < 0 ||
    m < n     = error "wrong number in pascal triangle."
  | n == m    = 1
  | otherwise = recursive_pascal (m - 1) (n - 1) +
                recursive_pascal (m - 1) n

-- The procedure computes factorial numbers by means of an iterative
-- process, with linear complexity.
fact_iter product counter maxc =
  if counter > maxc
  then product
  else fact_iter (counter * product) (counter + 1) maxc

factorial n = fact_iter 1 1 n

--
-- We can use the mathematical definition of combinatorial numbers to
-- simply computes them:
--
--                    m!                  factorial(m)
--     C(m, n) = ------------- = -------------------------------
--                n! (m - n)!    factorial(n) * factorial(m - n)
--
--                m * (m - 1) * ... * (m - n + 1)
--             = ---------------------------------
--                          factorial(n)
--
-- Thus we can write a new procedure in scheme and it is very simple.
-- Notice that:
--
--     fact_iter 1 a b = a * (a + 1) * ... * (b - 1) * b
--
-- We have: C(m, n) = fact-iter(1, m - n + 1, m) / fact-iter(1, 1, n)
-- This formula only does (2 * n - 1) times of multiplication / division.
--

-- Computes the combinatorial numbers
combinatorial :: (Eq a, Integral a) => a -> a -> a
combinatorial m n =
  if (n * 2) > m
  then (fact_iter 1 (n + 1) m) `div`
       (fact_iter 1 1 (m - n))
  else (fact_iter 1 (m - n + 1) m) `div`
       (fact_iter 1 1 n)

--
-- There also exists another algorithm, according to the formula given by
-- the Pascal's triangle:
--
--     C(m, n) = C(m - 1, n - 1) + C(m - 1, n)    (m, n >= 1)
--
-- We make a vector v with init value #(1, 0, 0, ... , 0) with length =
-- n + 1. and then make a new vector v':
--
--     v'(n) = v(n) + v(n - 1)   (1 <= n <= count)
--
-- where the variable `count` means the number of non-zero elements in the
-- vector v. After this operation, we get v' = #(1, 1, 0, ... , 0) with
-- length = n + 1. Continue doing such an operation on v', we get v'' =
-- #(1, 2, 1, ... , 0) and v''' = #(1, 3, 3, 1, ... , 0). We can get all
-- the elements on m th row in Pascal's triangle, using this algorithm.
-- Besides, the algorithm behaves well on complexity analysis, for we only
-- do O(m ^ 2) times of addition to compute all numbers C(m, k).
--
-- Now we realize this algorithm in Haskell...
--
--      _  _____ _____ _____ _   _ _____ ___ ___  _   _ 
--     / \|_   _|_   _| ____| \ | |_   _|_ _/ _ \| \ | |
--    / _ \ | |   | | |  _| |  \| | | |  | | | | |  \| |
--   / ___ \| |   | | | |___| |\  | | |  | | |_| | |\  |
--  /_/   \_\_|   |_| |_____|_| \_| |_| |___\___/|_| \_|
--
-- The vector package is needed here! Run
--
--     cabal install vector
--
-- first and use Vector package.
--

-- This function computes the next line (vector) in Pascal's triangle.
-- For example, it returns [1,5,10,10,5,1] if the input is [1,4,6,4,1].
pascal_updateVec :: (Eq a, Integral a) => V.Vector a -> V.Vector a
pascal_updateVec vector =
  V.zipWith (+) (V.snoc vector 0) (V.cons 0 vector)

-- This function computes the (n+1)-th line (vector) in Pascal's Triangle.
-- For example, it returns [1,5,10,10,5,1] if the input is 5.
pascalList :: (Eq a, Integral a) => a -> V.Vector a
pascalList n =
  let nextVec vector 0 = vector
      nextVec vector count =
        nextVec (pascal_updateVec vector) (count - 1)
      v0 = V.fromList [1 :: Integral a => a]
      in nextVec v0 n

-- The pascal number
pascal n m = (pascalList n) V.! m



