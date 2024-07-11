@a = external global i6

declare i6 @llvm.fshr.i6 (i6 %a, i6 %b, i6 %c)

define void @f(i6 %arg) {
  %r = call i6 @llvm.fshr.i6(i6 %arg, i6 %arg, i6 1)
  store i6 %r, ptr @a, align 1
  ret void
}
