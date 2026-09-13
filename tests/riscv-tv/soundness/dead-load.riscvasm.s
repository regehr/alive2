.text
.globl test
.type test,@function
test:
ld zero, 0(a0)
li a0, 42
ret
.Lfunc_end0:
.size test,.-test
