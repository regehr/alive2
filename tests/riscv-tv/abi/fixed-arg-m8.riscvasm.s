.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e64, m8, ta, ma
vxor.vv v8, v8, v16
ret
.Lfunc_end0:
.size test,.-test
