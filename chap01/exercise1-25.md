# using fast-expt in expmod

(expmod a n c) checks if a^n is congruent with c

It uses remainder repeatedly to reduce the size of the number being checked. 

Here's the new version of expmod. Let's use it to check if 1009 is prime. We'll choose 2 randomly as the a. If n is prime, a^n is congruent with a modulo n. 

```scheme
(define (expmod base exp m)
  (remainder (fast-expt base exp) m))

(define (fast-expt-iter b n a)
  (if (= n 0)
      a
      (if (even? n)
          (fast-expt-iter (* b b) (/ n 2) a)
          (fast-expt-iter b (- n 1) (* a b)))))

(define (fast-expt b n)
  (fast-expt-iter b n 1))
```

## the simulation
```scheme
(expmod 2 1009 1009)
(remainder (fast-expt 2 1009) 1009)
(remainder (fast-expt-iter 2 1009 1) 1009)
(remainder (fast-expt-iter 2 1008 2) 1009)
(remainder (fast-expt-iter 4 504 2) 1009)
(remainder (fast-expt-iter 16 252 2) 1009)
(remainder (fast-expt-iter 256 126 2) 1009)
(remainder (fast-expt-iter 65536 63 2) 1009)
(remainder (fast-expt-iter 65536 62 131072) 1009)
(remainder (fast-expt-iter 4.29 * 10^9 31 131072) 1009)
(remainder (fast-exp-iter 4.29 * 10^9 30 ...
```

Using fast-expt, for any prime that is at all large, the numbers being held by the fast-expt-iter procedure become large very quickly -- and that makes sense, because we have to raise those numbers to the n power.

The original expmod uses remainder again and again to reduce the size of the numbers we are checking.


