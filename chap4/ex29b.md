# memoization and state

Consider the following sequence

```
> (define count 0)
'ok

> (define (id x)
  (set! count (+ count 1))
  x)
'ok

> (define (square x) (* x x))

> (square (id 10))
100

> count
1
```

This turns out to be correct. 

Without memoization, id would have run twice and count would have been 
set to 2. 
But since (id 10) is memoized, it only runs once when passed to square, 
and the result is saved.

