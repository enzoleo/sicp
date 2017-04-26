## The solution of exercise 1.5
## What behavior will you observe with an interpreter that uses
## applicative-order evaluation? What behavior will you observe with
## an interpreter that uses normal-order evaluation?
##
## Assume that the evaluation rule for the special form `if` is the
## same whether the interpreter is using normal or applicative order:
## The predicate expression is evaluated first, and the result determines
## whether to evaluate the consequent or the alternative expression.
##

## Define a function named `p`.
## Notice this procedure has no formal parameters in its definition, and
## the body actually does nothing. So this procedure will call itself
## again and again without an end.
def p():
    return p()

def test(x, y):
    if x == 0:
        return 0
    else:
        return y

##
## Test the following code:
##
##     import sicpc1e05 as s5
##     s5.test(0, s5.p())
##
## and the interpreter raises an error:
## RecursionError: maximum recursion depth exceeded
##
    

