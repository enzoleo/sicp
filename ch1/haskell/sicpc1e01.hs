-- The solution of exercise 1.1
-- Give the result printed by the interpreter in response to each express-
-- ion. Assume that the sequence is to be evaluated in the order in which
-- it is presented.

a = 3
b = a + 1

result = do { print 10;
              print (5 + 3 + 4);
              print (9 - 1);
              print (6 / 2);
              print ((2 * 4) + (4 - 6));
              print (a + b + a * b);
              print (a == b);
              print (if b > a && b < a * b
                     then b            
                     else a);
              print (if a == 4
                     then 6
                     else if b == 4
                          then 6 + 7 + a
                          else 25);
              print (if b > a
                     then b + 2
                     else a + 2);
              print (let temp = a + 1
                     in if a > b
                        then temp * a
                        else if a < b
                             then temp * b
                             else temp * (-1)); }


