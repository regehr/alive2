@a = external global i5

declare i5 @llvm.fshr.i5 (i5 %a, i5 %b, i5 %c)

define void @f(i5 %arg) {
  %r = call i5 @llvm.fshr.i5(i5 %arg, i5 %arg, i5 1)
  store i5 %r, ptr @a, align 1
  ret void
}
