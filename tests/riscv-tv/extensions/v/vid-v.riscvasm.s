.text
.globl test
.type test,@function
test:
vsetivli zero, 3, e16, m1, ta, ma
vid.v v8
vse16.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
