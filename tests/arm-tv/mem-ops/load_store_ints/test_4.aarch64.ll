@a = external global i4

declare i4 @llvm.fshr.i4 (i4 %a, i4 %b, i4 %c)

define void @f() {
  %1 = load i4, ptr @a, align 1
  %r = call i4 @llvm.fshr.i4(i4 %1, i4 %1, i4 1)
  store i4 %r, ptr @a, align 1
  ret void
}
