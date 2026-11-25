.text
.globl test
.type test,@function
test:
vsetivli zero, 4, e16, m1, ta, ma
vle16.v v8, (a0)
vid.v v9
vrsub.vi v9, v9, 3
vrgather.vv v10, v8, v9
vse16.v v10, (a0)
ret
.Lfunc_end0:
.size test,.-test
