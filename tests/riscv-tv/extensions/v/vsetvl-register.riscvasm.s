.text
.globl test
.type test,@function
test:
vsetvl a0, a0, a1
ret
.Lfunc_end0:
.size test,.-test
