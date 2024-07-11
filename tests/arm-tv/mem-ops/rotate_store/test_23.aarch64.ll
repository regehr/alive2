@a = external global i23

declare i23 @llvm.fshr.i23 (i23 %a, i23 %b, i23 %c)

define void @f(i23 %arg) {
  %r = call i23 @llvm.fshr.i23(i23 %arg, i23 %arg, i23 1)
  store i23 %r, ptr @a, align 1
  ret void
}
