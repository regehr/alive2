.text
.globl test
.type test,@function
test:
csrr a0, vlenb
ret
.Lfunc_end0:
.size test,.-test
