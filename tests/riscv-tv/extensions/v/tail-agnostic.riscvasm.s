.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vmv.v.i v8, 1
vsetivli zero, 2, e32, m1, ta, mu
vmv.v.i v8, 7
vsetivli zero, 4, e32, m1, ta, ma
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
