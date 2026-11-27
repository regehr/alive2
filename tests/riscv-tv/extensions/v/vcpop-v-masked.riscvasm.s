.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e8, m1, ta, ma
vmv.v.i v0, 5
vsetivli zero, 4, e32, m1, ta, mu
vle32.v v8, (a1)
vle32.v v9, (a2)
vcpop.v v9, v8, v0.t
vse32.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
