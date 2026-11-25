.text
.globl test
.type test,@function
test:
vl1re8.v v8, (a1)
vmv1r.v v9, v8
vs1r.v v9, (a0)
ret
.Lfunc_end0:
.size test,.-test
