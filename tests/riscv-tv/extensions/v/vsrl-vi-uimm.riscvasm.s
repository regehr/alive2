.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e64, m1, ta, ma
vle64.v v8, (a1)
vsrl.vi v8, v8, 31
vse64.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
