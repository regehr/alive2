.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e8, m8, ta, ma
vrgatherei16.vv v8, v16, v24
ret
.Lfunc_end0:
.size test,.-test
