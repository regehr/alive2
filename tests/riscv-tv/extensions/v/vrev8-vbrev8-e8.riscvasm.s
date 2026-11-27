.text
.globl test
.type test,@function
test:
vsetivli zero, 16, e8, m1, ta, ma
vle8.v v8, (a2)
vrev8.v v9, v8
vbrev8.v v10, v8
vse8.v v9, (a0)
vse8.v v10, (a1)
ret
.Lfunc_end0:
.size test,.-test
