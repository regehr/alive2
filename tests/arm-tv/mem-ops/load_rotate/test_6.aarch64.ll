@a = external global i6

declare i6 @llvm.fshr.i6 (i6 %a, i6 %b, i6 %c)

define i6 @f() {
  %1 = load i6, ptr @a, align 1
  %r = call i6 @llvm.fshr.i6(i6 %1, i6 %1, i6 1)
  ret i6 %r
}
