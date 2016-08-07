#Own language interpreter in Haskell#
## Project created as an assignment for *Programming Languages and Paradigms* course ##

### Task ###
The purpose of this task is to design your own programming language and write an interpreter for it in Haskell.

### My language ###
I have decided to create an imperative language with C-like syntax. Here are some features of my language:

* **Types**: bool, int, void
* **Arithmetic and assignment**: +, -, *, /, ( ), variable++, variable--, =, +=, -=, *=, /=, comparing values
* **Loops**: while i for
* **Conditional instructions**: if, if { ... } else { ... } 
* **Functions**: recursion, function nesting (a possibility of declaring a function inside another function), static identifier binding, passing arguments by value, a possibility of returning void type (procedures).
* **Variable shadowing with static binding**
* **Static type checking**
* **Handing exceptions explicitly**, e.g.: division by 0, index out of bounds 
* **Printing values to standard output**: *print* instruction – it can print single variables, adds new line character
* **Arrays**: indexed with integers, each array needs to be assigned a size by using *init* instruction
* **Maps**: indexed with any comparable type
* **Structures**: a possibility to declare fields with name and later reading from them and assigning value to them

### Grade ###
For this assignment I got 24 out of 24 points.