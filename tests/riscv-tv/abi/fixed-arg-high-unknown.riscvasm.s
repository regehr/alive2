.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vslidedown.vi v8, v8, 2
ret
.Lfunc_end0:
.size test,.-test
