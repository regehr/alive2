@a = external global i10

declare i10 @llvm.fshr.i10 (i10 %a, i10 %b, i10 %c)

define void @f() {
  %1 = load i10, ptr @a, align 1
  %r = call i10 @llvm.fshr.i10(i10 %1, i10 %1, i10 1)
  store i10 %r, ptr @a, align 1
  ret void
}
