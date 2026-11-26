.text
.globl test
.type test,@function
test:
vsetvli a0, zero, e32, m1, ta, ma
srli a0, a0, 1
vslidedown.vx v8, v8, a0
ret
.Lfunc_end0:
.size test,.-test
