.text
.globl test
.type test,@function
test:
vsetivli zero, 1, e32, m1, ta, ma
vle32.v v8, (a1)
vle32.v v9, (a2)
vle32.v v11, (a3)
vle32.v v12, (a4)
vmadc.vv v0, v8, v9
vadd.vv v8, v8, v9
vadc.vvm v11, v11, v12, v0
vse32.v v8, (a0)
addi a0, a0, 4
vse32.v v11, (a0)
ret
.Lfunc_end0:
.size test,.-test
