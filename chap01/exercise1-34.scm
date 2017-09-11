(define (f g)
  (g 2))

; when (f f) is run, we get "The object 2 is not applicable" 
; I believe we get this error because f must take as an argument
; a procedure that can operate on the integer 2. 
; f is a procedure that only operates on other procedures
