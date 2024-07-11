@a = external global i61

declare i61 @llvm.fshr.i61 (i61 %a, i61 %b, i61 %c)

define void @f() {
  %1 = load i61, ptr @a, align 1
  %r = call i61 @llvm.fshr.i61(i61 %1, i61 %1, i61 1)
  store i61 %r, ptr @a, align 1
  ret void
}
