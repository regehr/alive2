.text
.globl test
.type test,@function
test:
addi sp, sp, -16
sd ra, 8(sp)
sd s0, 0(sp)
mv s0, a0
vsetivli zero, 4, e32, m1, ta, ma
call foo
vmv.v.i v8, 0
vse32.v v8, (s0)
ld ra, 8(sp)
ld s0, 0(sp)
addi sp, sp, 16
ret
.Lfunc_end0:
.size test,.-test
