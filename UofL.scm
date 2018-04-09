#lang racket
(require parser-tools/lex)

;Global Variables
(define quit "exit")
(define globalFuncStack '()) ;stack of functions
(define globalVariStack '()) ;stack of Variables '((var1 val1) (var2 val2) ... (varn valn))

;Clears both global Stacks
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
  (if(>(string-length input) 7)
     (cond ;then condition
       [(equal? (substring input 0 8) "#definev")(display "variable ")]
       [(equal? (substring input 0 8) "#definef")(display "function ")]
       );end cond
     (void);else void
     );end if
  (cond
    [(equal? input "#clear") (ClearStacks)];clears the memory ;COMPLETE
    [(equal? input "#exit")(set! quit #t)];exits the program ;COMPLETE
    [else (void)]
    );end cond
    

  
  (if (equal? quit #t); if #exit, quit.
      (display "Quitting UofL")
     (UofL);recursivly enter UofL
     );endif
    );END OF UOFL
(UofL)
