.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, m1, ta, ma
vzext.vf8 v9, v8
ret
.Lfunc_end0:
.size test,.-test
