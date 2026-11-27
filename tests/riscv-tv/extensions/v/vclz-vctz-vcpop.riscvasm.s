.text
.globl test
.type test,@function
test:
vsetivli zero, 8, e16, m1, ta, ma
vle16.v v8, (a3)
vclz.v v9, v8
vctz.v v10, v8
vcpop.v v11, v8
vse16.v v9, (a0)
vse16.v v10, (a1)
vse16.v v11, (a2)
ret
.Lfunc_end0:
.size test,.-test
