.text
.file 1 "foo.ll"
.globl test
.type test,@function
test:
addi sp, sp, -8
sd ra, 0(sp)
.loc 1 0 0
call check_alignment
ld ra, 0(sp)
addi sp, sp, 8
ret
.Lfunc_end0:
.size test,.-test
