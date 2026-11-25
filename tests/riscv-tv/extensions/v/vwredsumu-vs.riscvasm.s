.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vle8.v v8, (a0)
vsetivli zero, 1, e16, m1, ta, ma
vmv.s.x v9, zero
vsetivli zero, 4, e8, m1, ta, ma
vwredsumu.vs v9, v8, v9
vsetivli zero, 1, e16, m1, ta, ma
vmv.x.s a0, v9
ret
.Lfunc_end0:
.size test,.-test
