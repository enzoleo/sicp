## The solution of exercise 1.42
## Let f and g be two one-argument functions. The composition f after g is
## defined to be the function x -> f(g(x)). Define a procedure compose
## that implements composition. For example, if `inc` is a procedure that
## adds 1 to its argument,
##
##     ((compose square inc) 6)
##     49
##
## -------- (above from SICP)
##

## Define composition procedure
def compose(f, g):
  return lambda x: f(g(x))

## Necessary functions
def inc(x): return x + 1
def square(x): return x * x

#
# TEST
#
# >>> (compose(square, inc))(6)
# 49
#



