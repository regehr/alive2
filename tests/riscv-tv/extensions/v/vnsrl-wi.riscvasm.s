.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, m1, ta, ma
vle16.v v8, (a1)
vsetivli zero, 2, e8, mf2, ta, ma
vnsrl.wi v9, v8, 8
vse8.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
