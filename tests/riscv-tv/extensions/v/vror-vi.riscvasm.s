.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e8, m1, ta, ma
vle8.v v8, (a1)
vror.vi v8, v8, 11
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
