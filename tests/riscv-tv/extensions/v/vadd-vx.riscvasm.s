.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, m1, ta, ma
vmv.v.i v8, 5
vadd.vx v8, v8, a1
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
