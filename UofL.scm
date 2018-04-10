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

;DefineVariable
(define (defineVariable input)
  (define index 0)
  (set! index(stringSearch input #\space 0));start searching from position 0
  (set! globalVariStack
        (append globalVariStack
                (list (substring input 0 index);variable name
                      (substring input (+ 1 index) (string-length input));variable type
                      )
                );end append to globalVari
        );end set
  (set! globalVariStack (append globalVariStack (list 0)));set value of variable to default as 0
   (display globalVariStack)
  );end define

;Get Value From Variable
(define (getValue varName index)
  (if (equal? varName (list-ref globalVariStack index));check if the current iterated variable name is right
      (list-ref globalVariStack (+ index 2));if yes, return the value of the variable
      (getValue varName (+ index 3)));else go to the next variable name in the list
  )

;define function
(define (defineFunction input)
   (define index 0)
  (set! index(stringSearch input #\space 0));start searching from position 0
  (set! globalFuncStack
        (append globalFuncStack
                (list (substring input 0 index);Function name
                      (substring input (+ 1 index) (string-length input));variable type
                      )
                )
        );done set function name
  (display globalFuncStack)
  );END define function



;Search for character in a string
(define (stringSearch input findChar index)
  (if (equal? (string-ref input index) findChar) ;if input's char at position index is equal to findChar
      index                                    ;return index position
      (stringSearch input findChar (+ index 1))) ;else iterate through
  )

;UofL 
(define (UofL . x);creates a function without input
(define input "") ;input
  (display "UofL>")
  (set! input (read-line));reads input
  (if(>(string-length input) 7)
     (cond ;then condition
       [(equal? (substring input 0 8) "#definev")(defineVariable (substring input 12 (string-length input)))]
       [(equal? (substring input 0 8) "#definef")(defineFunction (substring input 12 (string-length input)))]
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

