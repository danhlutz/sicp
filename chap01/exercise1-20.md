# GCD - normal vs. applicative order    

## applicative order

```scheme
(gcd 206 40)
(gcd 40 6)
(gcd 6 4)
(gcd 4 2)
(gcd 2 0)
2
```

## normal order

(GCD 206 40)
(GCD 40 (remainder 206 40))
(GCD 40 6)
(GCD 6 (remainder 40 6))
(GCD 6 4)
(GCD 4 (remainder 6 4))
(GCD 4 2)
(GCD 2 (remainder 4 2))
(GCD 2 0)
2

## conclusion

I believe this completes in the same number of steps whether or not normal vs applicative order is used, because the remainder always reduces to an integer in one step even using normal order.
