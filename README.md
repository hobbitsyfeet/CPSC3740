# CPSC3740
This project creates a "Programmable Calculator"

Description:
UofL is the name of this "Calculator"
UofL is implemented as an interpreter for a language built in scheme via DrRacket.

Base Design:
UofL is a recursive function that will take an input and so long as the input isn't the exit code,
it will enter a new recursive layer and continue evaluating the inputs.
If the input is the "exit" code, then it will exit the recursion.

Functionalities:
Note: When <Word> is used, it describes what the input should be in that place.
Ex. <dataType> could be replaced with integer, boolean or float.
<boolean> is replaced with a variable with the type boolean.


Defining:
This defines a variable to later be used.
#definevari <variableName> 

#definefunc <funcName> <var_1> <var_2> ... <var_n>
    <expressions>
    #definefunc


Arithmetic Operators:
+  (addition) 
-  (subtraction) 
* (multiplication) 
/ (division) 
^ (power) 

The relational operators are
== (equal) 
<> (not equal) 
>=  (bigger than or equal to) 
<=  (smaller than or equal to) 
>  (bigger than) 
< (smaller than) 

The Data Types are:
integer (whole number 100)
float (floating point 0.01)
boolean (true/false)

Assignment Statement:
<variable> = <expression>

Selection Statment:
if(<boolean>)then 
        ...
elseif(<boolean>)then
        ...
endif

Iterative Statement:

for <intitial assignment> to <final number> do
        ...
endfor

Input Statement:
input <dataType>

Output Statement:
output <dataType>
Note: <dataType> must already be declred

UofL can evaluate and print out a given expression
<expression>
it will print out the evaluated expression.
