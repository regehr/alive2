.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vle8.v v8, (a1)
vmsne.vi v9, v8, 0
viota.m v10, v9
vse8.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
