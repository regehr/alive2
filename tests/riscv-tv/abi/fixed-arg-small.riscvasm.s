.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, mf8, ta, ma
vadd.vv v8, v8, v9
ret
.Lfunc_end0:
.size test,.-test
