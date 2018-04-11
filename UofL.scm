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
  (cond
    [(or (equal? input "I integer")(or (equal? input "I float")(equal? input"I boolean")))(display "Don't name anything PolyD ")]
    [(or (equal? input "J integer")(or (equal? input "J float")(equal? input"J boolean")))(display "Don't name anything PolyD ")]
    [else
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
     ;(display globalVariStack)
     ];end else
    );end conditional
  );end define

;Get Value From Variable
(define (getValue varName index)
  (if (equal? varName (list-ref globalVariStack index)) ;check if the current iterated variable name is right
      (list-ref globalVariStack (+ index 2));if yes, return the value of the variable
      (getValue varName (+ index 3)));else go to the next variable name in the list
  );endGetValue from Variable



;Variable is defined (returns true if input is a variable)
(define (variDefined varName index)
  (if (equal? varName (list-ref globalVariStack index));check if the current iterated variable name is right
      #t
      (variDefined varName (+ index 3)));else go to the next variable name in the list
      )

;Evaluate Expression
(define (evaluate input)
  (define splitLocation 0)
  (define operation 0)
  (cond
    [(foundCharacter input #\^ 0)(set! splitLocation (stringSearch input #\^ 0))]
    [(foundCharacter input #\/ 0)(set! splitLocation (stringSearch input #\/ 0))]
    [(foundCharacter input #\* 0)(set! splitLocation (stringSearch input #\* 0))]
    [(foundCharacter input #\+ 0)(set! splitLocation (stringSearch input #\+ 0))]
    [(foundCharacter input #\- 0)(set! splitLocation (stringSearch input #\- 0))]
    );end cond
  (if (not(equal? splitLocation 0));if splitlovation is not 0
      (set! operation (string-ref input splitLocation));set operation
      (void))
  ;(display "Set operation is now")
  ;(display operation)
  
  (if (not(equal? splitLocation 0));if splitlovation is not 0
      (evaluate (substring 0 (- 1 splitLocation)));split left of the value ASSUME SPACES
      (void))
  (if (not(equal? splitLocation 0))
      (evaluate (substring (+ 1 splitLocation) (string-length input)));split right
      (void))
      (string->number input);convert string value to integer value
  
  );end Evaluate expression

(define (assignVari input index value)
  (if(equal? (list-ref globalVariStack index) input)
     (set! globalVariStack (list-set globalVariStack (+ index 2) value))
     (assignVari input (+ index 3) value))
  )

;Do we want to have values passed in like so?
;QUESTION? "functionName" "varName1 varValue1 varName2 varValue2"
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
  (write globalFuncStack)
  );END define function

;Search for character in a string
(define (stringSearch input findChar index)
  (if (equal? (string-ref input index) findChar) ;if input's char at position index is equal to findChar
      index                                    ;return index position
      (stringSearch input findChar (+ index 1))) ;else iterate through
  )

(define (rightmostbracket input index)
  (if(equal? (string-ref input index) #\()
     index
     (rightmostbracket input (- index 1))
  ))

;foundCharacter (same as stringSearch but returns true/false instead of position)
(define (foundCharacter input findChar index)
  (cond
    [(equal? index (string-length input)) #f];reached the end return false
    [(equal? (string-ref input index) findChar)#t];reached goal char return true
    [else (foundCharacter input findChar (+ index 1))];keep searching
    )
   );end define foundCharacter

;UofL 
(define (UofL . x);creates a function without input
  (define input "") ;input
  (define test #t)
  (display "UofL>")
  (set! input (read-line));reads input
  (if(>(string-length input) 7)
     (cond ;then condition
       [(equal? (substring input 0 8) "#definev")(defineVariable (substring input 12 (string-length input)))(set! test #f)]
       [(equal? (substring input 0 8) "#definef")(defineFunction (substring input 12 (string-length input)))(set! test #f)]
       );end cond
     (void);else void
     );end if
  (if(>(string-length input) 6)
     (cond ;then condition
       [(equal? (substring input 0 6) "output")(display (getValue (substring input 7 (string-length input))0))(set! test #f)]
       );end cond
     (void);else void
     );end if
  (if(>(string-length input) 5)
     (cond
       [(equal? (substring input 0 5) "input")(assignVari (substring input 6 (+ (stringSearch (substring input 6 (string-length input)) #\space 0) 6)) 0 (substring input (+ (stringSearch (substring input 6 (string-length input)) #\space 0) 6) (string-length input)))];trust me, it works -Chris
       )
     (void)
     )
  (cond
    [(equal? input "#clear") (ClearStacks)];clears the memory ;COMPLETE
    [(equal? input "#exit")(set! quit #t)];exits the program ;COMPLETE
    ;[(not (equal? (string-ref input 0) #\#))(variDefined (substring input 0 (stringSearch input #\space 0)) 0)]
    [else (evaluate input)]
    );end cond
  (if(equal? #t test)
     (if(and (variDefined (substring input 0 (stringSearch input #\space 0)) 0) (equal? (string-ref input (+ (stringSearch input #\space 0) 1)) #\=))
        (assignVari (substring input 0 (stringSearch input #\space 0)) 0 (substring input (+ (stringSearch input #\space 0) 3) (string-length input)))
        (void))
  (void))
    

  
  (if (equal? quit #t); if #exit, quit.
      (display "Quitting UofL")
     (UofL);recursivly enter UofL
     );endif
    );END OF UOFL

(UofL)

