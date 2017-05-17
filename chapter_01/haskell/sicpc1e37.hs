-- The solution of exercise 1.37
-- [a] An infinite continued fraction is an expression of the form:
--
--                       N_1
--         f = -------------------------
--                           N_2
--             D_1 + -------------------
--                              N_3
--                    D_2 + ------------
--                                  N_4
--                           D_3 + -----
--                                  ...
--
--     As an example, one can show that the infinite continued fraction
--     expansion with the N_i and the D_i all equal to 1 produces 1 / \phi,
--     where \phi is the golden ratio (described in section 1.2.2). One way
--     to approximate an infinite continued fraction is to truncate the
--     expansion after a given number of terms. Such a truncation -- a
--     so-called `k-term finite continued fraction` -- has the form:
--
--                 N_1
--         --------------------
--                    N_2
--         D_1 + --------------
--                        N_k
--                ...  + -----
--                        D_k
--
--     Suppose that `n` and `d` are procedures of one argument (the term
--     index i) that return the N_i and D_i of the terms of the continued
--     fraction. Define a procedure cont-frac such that evaluating `(cont-
--     frac n d k)` computes the value of the k-term finite continued
--     fraction. Check your procedure by approximating 1 / \phi using
--
--         (cont-frac (lambda (i) 1.0)
--                    (lambda (i) 1.0)
--                    k)
--
--     for successive values of `k`. How large must you make k in order to
--     get an approximation that is accurate to 4 decimal places?
--
-- [b] If your cont-frac procedure generates a recursive process, write
--     one that generates an iterative process. If it generates an
--     iterative process, write one that generates a recursive process.
--
-- -------- (above from SICP)
--

-- The `cont-frac` procedure as a recursive process
contFrac
  :: (Eq a, Num a, Fractional b) =>
     (a -> b) -> (a -> b) -> a -> b
contFrac n d k =
  let iter counter =
        (n counter) / (d counter + if k == counter
                                   then 0
                                   else iter (counter + 1))
  in iter 1

-- The `cont-frac` procedure as a iterative process
contFracIter
  :: (Eq a, Num a, Fractional b) =>
     (a -> b) -> (a -> b) -> a -> b
contFracIter n d k =
  let iter counter result
        | counter == 0 = result
        | otherwise    = iter (counter - 1)
                              (n counter / (d counter + result))
  in iter k 0

-- The recursive process to compute golden ratio
golden_ratio :: (Eq a, Num a, Fractional b) => a -> b
golden_ratio k =
  contFrac (\x -> 1.0) (\x -> 1.0) k

-- The iterative process to compute golden ratio
golden_ratioIter :: (Eq a, Num a, Fractional b) => a -> b
golden_ratioIter k =
  contFracIter (\x -> 1.0) (\x -> 1.0) k

--
-- At least 4 items needed to get an approximation that is accurate to 4
-- decimal places:
--
-- Input items k: 9
-- [  The exact value   ] 0.6180339887498949
-- [ Result (iterative) ] 0.6181818181818182
-- [ Result (recursive) ] 0.6181818181818182
--
-- Input items k: 10
-- [  The exact value   ] 0.6180339887498949
-- [ Result (iterative) ] 0.6179775280898876
-- [ Result (recursive) ] 0.6179775280898876
--
-- Input items k: 11
-- [  The exact value   ] 0.6180339887498949
-- [ Result (iterative) ] 0.6180555555555556
-- [ Result (recursive) ] 0.6180555555555556
--


