.text
.globl test
.type test,@function
test:
vsetivli zero, 8, e32, m2, ta, ma
vadd.vv v8, v8, v10
ret
.Lfunc_end0:
.size test,.-test
