@a = external global i10

declare i10 @llvm.fshr.i10 (i10 %a, i10 %b, i10 %c)

define i10 @f() {
  %1 = load i10, ptr @a, align 1
  %r = call i10 @llvm.fshr.i10(i10 %1, i10 %1, i10 1)
  ret i10 %r
}
