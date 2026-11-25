.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, mf8, ta, ma
vmv.v.i v8, 1
vsetivli zero, 1, e8, mf8, ta, mu
vmv.v.i v8, 7
vsetivli zero, 2, e8, mf8, ta, ma
vse8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
