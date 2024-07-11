@a = external global i4

declare i4 @llvm.fshr.i4 (i4 %a, i4 %b, i4 %c)

define void @f(i4 %arg) {
  %r = call i4 @llvm.fshr.i4(i4 %arg, i4 %arg, i4 1)
  store i4 %r, ptr @a, align 1
  ret void
}
