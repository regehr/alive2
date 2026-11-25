.text
.globl test
.type test,@function
test:
beqz a1, .LBB0_2
vsetivli zero, 4, e32, m1, ta, ma
j .LBB0_3
.LBB0_2:
vsetivli zero, 4, e32, m1, ta, ma
.LBB0_3:
vmv.v.i v8, 0
vse32.v v8, (a0)
ret
.Lfunc_end0:
.size test,.-test
