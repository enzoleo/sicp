-- The solution of exercise 1.13
-- Prove that Fib(n) is the closest integer to phi ^ n / sqrt(5), where
-- phi = (1 + sqrt(5)) / 2. Hint: Let psi = (1 - sqrt(5)) / 2, then
-- use induction and the definition of the Fibonacci numbers (see section
-- 1.2.2) to prove that Fib(n) = (phi ^ n - psi ^ n) / sqrt(5)
--
-- This exercise is a pure mathematical problem. The proof is not difficult
-- with the recursion formula of this sequence. Here we compute Fibonacci
-- numbers and do simple comparison.
--

-- Compute Fibonacci numbers (linear steps)
-- There exists an algorithm for computing the Fibonacci numbers in a
-- logarithmic number of steps. (See Exercise 1.19)
fib :: (Eq a, Integral a) => a -> a
fib n =
  if n < 0
  then error "illegal index"
  else
    let iter a b 0 = b
        iter a b count =
          iter (a + b) a (count - 1)
    in iter 1 0 n

showFib :: (Integral a) => a -> IO ()
showFib n =
  if n < 0
  then error "illegal number"
  else
    let phi = (1 + sqrt 5) / 2
        phiExpt m =
          phi ^ m / sqrt 5
        singleShow m 0 = return ()
        singleShow m count =
          do putStrLn ("fibonacci(" ++ (show m) ++ ") \t" ++
                       (show $ fib m) ++ "  \tphi-expt(" ++
                       (show m) ++ ") \t" ++ (show $ phiExpt m))
             singleShow (m + 1) (count - 1)
    in singleShow 1 n


