## Latte
This is latte to ASM compiler.
Latte has syntax that is very similar to C++/Java.
It has all the basic stuff + classes, arrays, functions with recurrence,
garbage collector and some optimizations.
Example programs are in ./good
Grammar description is in Latte.cf (in bfnc format, https://bnfc.readthedocs.io/en/latest/lbnf.html)



### How to run
run make in main directory

You can use ./latc_x86_64 to create ASM and binary files from .lat program, for example
./latc_x86_64 good/function_few_arguments.lat will create two files in ./programs:
- asm file function_few_arguments.s
- binary file function_few_arguments



### How does it work
code -> syntax tree -> list of quadruples -> split into blocks -> calculate CFG ->
analyze living varibles -> ASM (special datatype) -> real asm as string
