.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e32, m1, ta, ma
vle32.v v8, (a0)
vrgather.vx v9, v8, a1
vse32.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
