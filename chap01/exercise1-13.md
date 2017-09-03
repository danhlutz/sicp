# Exercise 1.13

Prove by induction that Fib(n) = ( Phi ** n - Psi ** n ) / (sqrt 5)

Phi = (1 + sqrt 5) / 2
Psi = (1 - sqrt 5) / 2

## Base case

For n = 0

Fib(0) = 0. By definition. 

Phi ** 0 - Psi ** 0 = 0. This is equal to Fib(0)

## Inductive Case

We will assume that Fib(n) = (Phi ** 2 - Psi ** 2 ) / (sqrt 5) in order to sshow that this implies Fib(n + 1) = (Phi ** (n+1) - Psi ** (n+1) ) / (sqrt 5)

Fib(n + 1) = (Phi ** (n + 1) - Psi ** (n + 1) / (sqrt 5)

Next, separate each term to take the two constants to the (n - 1) power.

Fib(n + 1) = ( (Phi ** 2) (Phi ** (n - 1)) - (Psi ** 2) (Psi ** (n - 1))) / (sqrt 5)

(Phi ** 2) = 1 + (1 + sqrt 5) / 2 = 1 + Phi
(Psi ** 2) = 1 + (1 - sqrt 5) / 2 = 1 + Psi

Now substitute in these values to get:
Fib(n + 1) = (Phi ** n + Phi ** (n-1) - Psi ** n - Psi ** (n-1)) / sqrt 5

Now reduce this algebraically
Fib(n + 1) = (Phi ** n - Psi ** n) / sqrt 5 +
             (Phi ** (n - 1) - Psi ** (n - 1)) / sqrt 5
This is equivalent to Fib(n) + Fib(n-1), which is the defition of Fib. 


