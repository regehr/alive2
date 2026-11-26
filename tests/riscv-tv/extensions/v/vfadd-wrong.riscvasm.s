.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, mf2, ta, ma
vfsub.vv v8, v8, v9
ret
.Lfunc_end0:
.size test,.-test
