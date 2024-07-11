@a = external global i13

declare i13 @llvm.fshr.i13 (i13 %a, i13 %b, i13 %c)

define void @f(i13 %arg) {
  %r = call i13 @llvm.fshr.i13(i13 %arg, i13 %arg, i13 1)
  store i13 %r, ptr @a, align 1
  ret void
}
