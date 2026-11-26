.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vadd.vv v1, v8, v8
vmv1r.v v8, v1
ret
.Lfunc_end0:
.size test,.-test
