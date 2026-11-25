.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m1, ta, ma
vle8.v v8, (a0)
vmseq.vi v9, v8, 0
vmsbf.m v10, v9
vcpop.m a0, v10
ret
.Lfunc_end0:
.size test,.-test
