# using square in expmod

Here's the old expmod

```scheme
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
```

And here's the new one

```scheme
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
          (remainder (* base (expmod base (- exp 1) m))
                     m))))
```

Now let's run a little simulation

```scheme
(expmod 2 4 4)
(remainder (* (expmod 2 2 4) (expmod 2 2 4)) 4)
(remainder (* (remainder (* (expmod 2 1 4) (expmod 2 1 4))) (expmod 2 2 4))
           4)
```

You can see that even with this example, we repeat the same sub-problems over and over again. 
