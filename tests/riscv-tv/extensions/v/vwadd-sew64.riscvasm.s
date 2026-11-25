.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e64, m1, ta, ma
vwadd.vv v8, v10, v12
ret
.Lfunc_end0:
.size test,.-test
