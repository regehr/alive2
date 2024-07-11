@a = external global i58

declare i58 @llvm.fshr.i58 (i58 %a, i58 %b, i58 %c)

define i58 @f() {
  %1 = load i58, ptr @a, align 1
  %r = call i58 @llvm.fshr.i58(i58 %1, i58 %1, i58 1)
  ret i58 %r
}
