@a = external global i88

declare i88 @llvm.fshr.i88 (i88 %a, i88 %b, i88 %c)

define void @f() {
  %1 = load i88, ptr @a, align 1
  %r = call i88 @llvm.fshr.i88(i88 %1, i88 %1, i88 1)
  store i88 %r, ptr @a, align 1
  ret void
}
