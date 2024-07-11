@a = external global i58

declare i58 @llvm.fshr.i58 (i58 %a, i58 %b, i58 %c)

define void @f(i58 %arg) {
  %r = call i58 @llvm.fshr.i58(i58 %arg, i58 %arg, i58 1)
  store i58 %r, ptr @a, align 1
  ret void
}
