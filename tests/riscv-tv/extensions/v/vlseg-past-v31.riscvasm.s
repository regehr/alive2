.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m1, ta, ma
vlseg4e8.v v30, (a0)
ret
.Lfunc_end0:
.size test,.-test
