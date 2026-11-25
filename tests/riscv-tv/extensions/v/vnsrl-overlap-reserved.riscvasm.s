.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vnsrl.wi v9, v8, 0
ret
.Lfunc_end0:
.size test,.-test
