.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vadd.vv v8, v0, v9, v0.t
ret
.Lfunc_end0:
.size test,.-test
