#lang racket
(require parser-tools/yacc parser-tools/lex;for lexer and parser functions
         (prefix-in ~ parser-tools/lex-sre);for arithmetic operators
         )

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

;Evaluate Expressions
(define-tokens tokensToParse (number));tokens to parse is defined as a number
(define-empty-tokens operators (openBracket closeBracket + - * / ^ == <> >= <= < > endFile negative)) ;define operators to use on numbers

;lexstring lexifies arithmetic and relational operators
(define lexString
  (lexer [(eof) 'endFile]
         [whitespace (lexString input-port)]
         [(~or "+" "-" "*" "/" "^") (string->symbol lexeme)];charset complement of all OR'd Arithmetics
         [(~or "==" "<>" ">=" "<=" ">" "<") (string->symbol lexeme)];charset complement of all OR'd Relationals
         ["(" 'openBracket ]
         [")" 'closeBracket]
         ;SRE Operators
         ;character set complement match exactly one char to both forms of...
         [(~:
           (~+ numeric);repeated numeric
           (~? ;the non occurence of...
            (~: #\. (~* numeric)));charset complement of both char '.' and complement of repeated numeric sequence 0 or more times 
           )
          (token-number (string->number lexeme))];end SRE Operators
         ));End define lexString
 ;parseline parses the line and defines grammar, precedence and operations for lexified operators
(define parseline
  (parser [start exp] [end endFile]
          [tokens tokensToParse operators]
          [error void]
          [precs (left - +) (left * /) (left negative)
                 (left ^) (left ==)
                 (left >=) (left <=)
                 (left <) (left >)
                 (nonassoc <>)] ;precidence (left = left associative)
          [grammar (exp [(number) $1] ;exp is a number
                      ;Start Arithmetic
                      [(exp + exp) (+ $1 $3)]
                      [(exp - exp) (- $1 $3)]
                      [(exp * exp) (* $1 $3)]
                      [(exp / exp) (/ $1 $3)]
                      [(exp ^ exp) (expt $1 $3)]
                      ;Start Relationals
                      [(exp == exp) (equal? $1 $3)]
                      [(exp <> exp) (not (equal? $1 $3))]
                      [(exp >= exp) (or (> $1 $3) (equal? $1 $3))]
                      [(exp <= exp) (or (< $1 $3) (equal? $1 $3))]
                      [(exp < exp) (< $1 $3)]
                      [(exp > exp) (> $1 $3)]
                      ;Negate number
                      [(- exp) (negNum negative) (- $2)]
                      ;evaluate within brackets
                      [(openBracket exp closeBracket) $2])]
          )
  )

;evaluates input and returns value of that evaluated input
(define (evaluate input)
  (define in (open-input-string input))
  (parseline (lambda() (lexString in))))

;Assigns a variable to given value
(define (assignVari input index value)
  (if(equal? (list-ref globalVariStack index) input);if variable name is equal to var at index
     (set! globalVariStack (list-set globalVariStack (+ index 2) value)) ;set the variable's value
     (assignVari input (+ index 3) value));else search the next variable
  )

;define function
(define (defineFunction input)
  (define index 0)
  (set! index(stringSearch input #\space 0));start searching from position 0
  (set! globalFuncStack
        (append globalFuncStack
                (list (substring input 0 index);Function name
                      (substring input (+ 1 index) (string-length input));variable type
                      );done list
                );end append
        );done set function name
  (displayln globalFuncStack);displays the stack of all functions
  );END define function


;Search for character in a string
(define (stringSearch input findChar index)
  (if (equal? (string-ref input index) findChar) ;if input's char at position index is equal to findChar
      index                                    ;return index position
      (stringSearch input findChar (+ index 1))) ;else iterate through
  )

;foundCharacter (same as stringSearch but returns true/false instead of position)
(define (foundCharacter input findChar index)
  (cond
    [(equal? index (string-length input)) #f];reached the end return false
    [(equal? (string-ref input index) findChar)#t];reached goal char return true
    [else (foundCharacter input findChar (+ index 1))];keep searching
    )
   );end define foundCharacter

;Iterates through a string and replaces all occurances of a variable with its value
(define (replace input index)
  ;(display input)
  ;(display index)
  (if(> index (- (length globalVariStack) 1))
     input
     (string-replace (replace input (+ index 3)) (list-ref globalVariStack index) (list-ref globalVariStack (+ index 2))))
  )

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
       [(equal? (substring input 0 6) "output")(displayln (getValue (substring input 7 (string-length input))0))(set! test #f)]
       );end cond
     (void);else void
     );end if
  (if(>(string-length input) 5)
     (cond
       [(equal? (substring input 0 5) "input")(assignVari (substring input 6 (+ (stringSearch (substring input 6 (string-length input)) #\space 0) 6)) 0 (substring input (+ (stringSearch (substring input 6 (string-length input)) #\space 0) 6) (string-length input)))(set! test #f)];trust me, it works -Chris
       )
     (void);else do nothing
     )
  (if (equal? test #t)
  (cond
    [(equal? input "#clear") (ClearStacks)(set! test #f)];clears the memory ;COMPLETE
    [(equal? input "#exit")(set! quit #t)(set! test #f)];exits the program ;COMPLETE
    [(not(or (foundCharacter input #\= 0) (foundCharacter input #\# 0))) (displayln (evaluate input))(set! test #f)]
    ;[(not (equal? (string-ref input 0) #\#))(variDefined (substring input 0 (stringSearch input #\space 0)) 0)(set! test #f)]
    ;[else (calc input )]
    );end cond
  (void))
  ;(set! test #f)
  ;(if(equal? #t test)
   ;  (set! input (string-append (substring input 0 (+(stringSearch input #\space 0)1)) (replace (substring input (+(stringSearch input #\space 0)1) (string-length input)) 0)))
    ; (void))
  ;(display input)
  
  (if(equal? #t test)
     ;replace all variables with their data
      (if(and (variDefined (substring input 0 (stringSearch input #\space 0)) 0) (equal? (string-ref input (+ (stringSearch input #\space 0) 1)) #\=))
        ;(display (calc(substring input (+ (stringSearch input #\space 0) 3) (string-length input))))
        (assignVari (substring input 0 (stringSearch input #\space 0)) 0 (number->string (evaluate(substring input (+ (stringSearch input #\space 0) 3) (string-length input)))))
        (void)
        );end if and     
     (void));end if equal?
  ;(set! test #t)
  (if (equal? quit #t); if #exit, quit.
      (display "Quitting UofL")
     (UofL);recursivly enter UofL
     );endif
    );END OF UOFL

(UofL)

