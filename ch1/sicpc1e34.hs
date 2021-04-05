-- The solution of exercise 1.34
-- Suppose we define the procedure:
--
--     (define (f g)
--       (g 2))
--
-- Then we have:
--
--     > (f square)
--     > 4
--
--     > (f (lambda (z) (* z (+ z 1))))
--     > 6
--
-- What happens if we (perversely) ask the interpreter to evaluate
-- the combination (f f)? Explain.
--
-- -------- (above from SICP)
--

-- Here we let GHCI infer param type by itself
fun g = g 2

--
-- We test `fun fun` in GHCI:
--
-- *Main> fun fun
--
-- <interactive>:16:1:
--     Non type-variable argument in the constraint: Num (a -> t)
--     (Use FlexibleContexts to permit this)
--     When checking that ‘it’ has the inferred type
--       it :: forall t a. (Num a, Num (a -> t)) => t
--
-- Show the param type of fun:
--
-- *Main> :t fun
-- fun :: Num a => (a -> t) -> t
--
-- This function needs an function argument g :: Num a => a -> t,
-- the argument of which is a number. Obviously, @fun cannot take
-- itself as an argument.
--



