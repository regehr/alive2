.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e32, m1, ta, ma
vle32.v v8, (a1)
vle32.v v9, (a2)
vsoxei32.v v8, (a0), v9
ret
.Lfunc_end0:
.size test,.-test
