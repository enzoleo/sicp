-- The solution of exercise 1.41
-- Define a procedure `double` that takes a procedure of one argument as
-- argument and returns a procedure that applies the original procedure
-- twice. For example, if `inc` is a procedure that adds 1 to its argument,
-- then `(double inc)` should be a procedure that adds 2. What value is
-- returned by
--
--     (((double (double double)) inc) 5)
--
-- -------- (above from SICP)
--

-- Notice that procedure `f` only has one argument
double f = \x -> f $ f x

-- Increase one number by 1
inc x = x + 1

--
-- Let's do a simple test.
--
-- *Main> inc 0
-- 1
-- *Main> double inc 0
-- 2
-- *Main> double double inc 0
-- 4
-- *Main> double double double inc 0
-- 16
-- *Main> double double double double inc 0
-- 65536
--
-- How to explain that...?
-- Obviously, `double f` returns a function which applies procedure `f`
-- twice. Thus `double double` returns a function which applies `double`
-- twice, which means `double double f` returns a function which applies
-- procedure `f` four times. We rename the procedure `double double`
-- `fourTimes`, then `double double double` equals to `fourTimes double`
-- which continously applies `double` fourtimes. So `double double double
-- f` continously applies f 2 ^ 4 = 16 times. We rename this procedure
-- `sixteenTimes`. Similarly, `double double double double f` = `sixteen
-- double f` applies f 2 ^ 16 = 65536 times.
--
-- *Main> double double double inc 5
-- 21
--



