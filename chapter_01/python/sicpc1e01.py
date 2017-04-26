## The solution of exercise 1.1
## Give the result printed by the interpreter in response to each express-
## ion. Assume that the sequence is to be evaluated in the order in which
## it is presented.

## Define some variables used later
a = 3
b = a + 1

result = [0] * 11
result[0] = 10
result[1] = 5 + 3 + 4
result[2] = 9 - 1
result[3] = 6 / 2
result[4] = ((2 * 4) + (4 - 6))
result[5] = (a + b + a * b)
result[6] = (a == b)
if b > a and b < a * b:
    result[7] = b
else:
    result[7] = a
    
if a == 4:
    result[8] = 6
elif b == 4:
    result[8] = 6 + 7 + a
else:
    result[8] = 25
    
if b > a:
    result[9] = b + 2
else:
    result[9] = a + 2
    
temp = 0
if a > b:
    temp = a
elif a < b:
    temp = b
else:
    temp = -1
result[10] = temp * (a + 1)


