#lang racket
(require parser-tools/lex)


(define quit "exit")
(define globalFuncStack '()) ;stack of functions
(define globalVariStack '()) ;stack of Variables

(define (ClearStacks)
  (display "Clearing Functions and Variables ")
  (set! globalFuncStack '()) ;empties global functions
  (set! globalVariStack '()) ;empties global variables
  )

;UofL 
(define (UofL . x);creates a function without input
(define input "") ;input
  (display "UofL>")
  (set! input (read-line));reads input
  
  (cond
    [(equal? input "#clear") (ClearStacks)];clears the memory ;COMPLETE
    [(equal? input "#exit")(set! quit #t)];exits the program ;COMPLETE
    
    [else (display "Invalid input ")] ;if none of the above, invalid
    );end cond
  
  (if (equal? quit #t); if #exit, quit.
      (display "Quitting UofL")
     (UofL);recursivly enter UofL
     );endif
    );END OF UOFL
(UofL)
