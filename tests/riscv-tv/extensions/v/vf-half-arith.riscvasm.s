.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, mf4, ta, ma
vfadd.vv v8, v8, v9
ret
.Lfunc_end0:
.size test,.-test
