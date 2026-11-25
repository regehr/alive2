.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e16, m1, ta, ma
vlseg2e16.v v8, (a1)
vse16.v v8, (a0)
vse16.v v9, (a2)
ret
.Lfunc_end0:
.size test,.-test
