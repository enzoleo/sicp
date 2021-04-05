## The solution of exercise 1.23
## The `smallest-divisor` procedure shown at the start of this section does
## lots of needless testing: After it checks to see if the number is
## divisible by 2 there is no point in checking to see if it is divisible
## by any larger even numbers. This suggests that the values used for
## `test-divisor` should not be 2, 3, 4, 5, 6, ..., but rather 2, 3, 5, 7,
## 9,... . To implement this change, define a procedure `next` that returns
## 3 if its input is equal to 2 and otherwise returns its input plus 2.
## Modify the `smallest-divisor` procedure to use (next test-divisor)
## instead of (+ test-divisor 1). With `timed-prime-test` incorporating
## this modified version of `smallest-divisor`, run the test for each of
## the 12 primes found in exercise 1.22. Since this modification halves the
## number of test steps, you should expect it to run about twice as fast.
## Is this expectation confirmed? If not, what is the observed ratio of the
## speeds of the two algorithms, and how do you explain the fact that it is
## different from 2?
##
## -------- (above from SICP)
##

import sys
import time

def isprime(n):
    """Check whether a number is a prime"""
    if not (isinstance(n, int) and n >= 2):
        return False
    if n == 2:
        return True
    if n % 2 == 0:
        return False
    a = 3
    while n % a:
        a += 2
        if a * a >= n:
            return True
    return False

def search_odd_primes(m, n):
    """This function searches all odd primes between lower bound and upper
    bound and return a sequence.
    """
    seq = []
    for number in range(m, n):
        if isprime(number):
            seq.append(number)
    return seq

def next_odd_prime(n):
    """Find the smallest odd prime larger than n"""
    if n % 2:
        number = n + 2
    else:
        number = n + 1
    while not isprime(number):
        number += 2
    return number

def next_odd_primes(n, k):
    """Find the k smallest primes larger than n"""
    if not isinstance(k, int):
        raise TypeError("only positive integer is allowed to be "
                        "numbers of primes: %s" % k)
    if k <= 0:
        raise ValueError("only positive integer is allowed to be "
                         "numbers of primes: %s" % k)
    seq = []
    for i in range(k):
        n = next_odd_prime(n)
        seq.append(n)
    return seq

def runtime_test(n, k):
    start = time.clock()
    seq = next_odd_primes(n, k)
    end = time.clock()
    return "runtime: %fs" % (end - start)
    
#
# RUNTIME TEST
#
#   >>> from sicpc1e23 import *
#   >>> runtime_test(10000000000, 100)
#   'runtime: 0.895185s'
#   >>> runtime_test(100000000000, 100)
#   'runtime: 2.666637s'
#   >>> runtime_test(1000000000000, 100)
#   'runtime: 8.617430s'
#


