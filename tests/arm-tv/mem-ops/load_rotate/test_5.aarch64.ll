@a = external global i5

declare i5 @llvm.fshr.i5 (i5 %a, i5 %b, i5 %c)

define i5 @f() {
  %1 = load i5, ptr @a, align 1
  %r = call i5 @llvm.fshr.i5(i5 %1, i5 %1, i5 1)
  ret i5 %r
}
