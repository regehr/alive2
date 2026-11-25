.text
.globl test
.type test,@function
test:
vsetivli zero, 2, e8, m4, ta, ma
vlseg3e8.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
