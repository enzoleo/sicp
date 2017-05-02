-- The solution of exercise 1.5
-- What behavior will you observe with an interpreter that uses
-- applicative-order evaluation? What behavior will you observe with
-- an interpreter that uses normal-order evaluation?
--
-- Assume that the evaluation rule for the special form `if` is the
-- same whether the interpreter is using normal or applicative order:
-- The predicate expression is evaluated first, and the result determines
-- whether to evaluate the consequent or the alternative expression.
--

-- Define a function named `p`.
-- Notice this procedure has no formal parameters in its definition, and
-- the body actually does nothing. So this procedure will call itself
-- again and again without an end.
p = p

test x y = if x == 0
           then 0
           else y

--
-- Test the following code:
--
--     :l sicpc1e05
--     test 0 p
--
-- and you can find...
--
--         _  _____ _____ _____ _   _ _____ ___ ___  _   _ 
--        / \|_   _|_   _| ____| \ | |_   _|_ _/ _ \| \ | |
--       / _ \ | |   | | |  _| |  \| | | |  | | | | |  \| |
--      / ___ \| |   | | | |___| |\  | | |  | | |_| | |\  |
--     /_/   \_\_|   |_| |_____|_| \_| |_| |___\___/|_| \_|
--
-- it returns zero.
--
--     Prelude> :l sicpc1e05.hs
--     [1 of 1] Compiling Main             ( sicpc1e05.hs, interpreted )
--     Ok, modules loaded: Main.
--     *Main> test 0 p
--     0
--
    

