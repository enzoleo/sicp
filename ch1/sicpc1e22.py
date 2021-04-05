## The solution of exercise 1.22
## Most Lisp implementations include a primitive called `runtime` that
## returns an integer that specifies the amount of time the system has been
## running (measured, for example, in microseconds).
##
## Write a procedure `search-for-primes` that checks the primality of
## consecutive odd integers in a specified range. Use your procedure to
## find the three smallest primes larger than 1000; larger than 10,000;
## larger than 100,000; larger than 1,000,000. Note the time needed to test
## each prime.
##
## Since the testing algorithm has order of growth of O(sqrt(n)), you
## should expect that testing for primes around 10,000 should take about
## sqrt(10) times as long as testing for primes around 1000. Do your timing
## data bear this out? How well do the data for 100,000 and 1,000,000
## support the sqrt(n) prediction? Is your result compatible with the
## notion that programs on your machine run in time proportional to the
## number of steps required for the computation?
##
## -------- (above from SICP)
##

import sys
import time

def smallest_divisor(n):
    """Find the smallest divisor of the given number n.
    Only positive integer input is allowed.
    
    For example, smallest_divisor(77) = 7.
    This function is used to check whether a number is a prime."""
    if not isinstance(n, int):
        raise TypeError("only positive integer has smallest divisor")
    if n <= 0:
        raise ValueError("only positive integer has smallest divisor")
    if n == 1:
        return 1
    a = 2
    while n % a:
        a += 1
        if a * a >= n:
            return n
    return a

def isprime(n):
    """Check whether a number is a prime"""
    if not (isinstance(n, int) and n >= 2):
        return False
    if n == 2:
        return True
    a = 2
    while n % a:
        a += 1
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
#   >>> from sicpc1e22 import *
#   >>> runtime_test(10000000000, 100)
#   'runtime: 1.852061s'
#   >>> runtime_test(100000000000, 100)
#   'runtime: 5.416295s'
#   >>> runtime_test(1000000000000, 100)
#   'runtime: 17.537827s'
#


