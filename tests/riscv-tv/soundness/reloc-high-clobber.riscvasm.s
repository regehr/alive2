.text
.globl test
.type test,@function
test:
li a0, 42
lui a0, %hi(g)
ret
.Lfunc_end0:
.size test,.-test
