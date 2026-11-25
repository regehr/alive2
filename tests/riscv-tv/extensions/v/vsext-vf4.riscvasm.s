.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, m1, ta, ma
vle8.v v8, (a1)
vsext.vf4 v9, v8
vse32.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
