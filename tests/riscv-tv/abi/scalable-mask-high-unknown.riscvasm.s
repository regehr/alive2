.text
.globl test
.type test,@function
test:
vsetvli a1, zero, e8, m8, ta, ma
vcpop.m a0, v0
vsetvli a1, zero, e8, mf8, ta, ma
vcpop.m a2, v0
sub a0, a0, a2
ret
.Lfunc_end0:
.size test,.-test
