@a = external global i61

declare i61 @llvm.fshr.i61 (i61 %a, i61 %b, i61 %c)

define i61 @f() {
  %1 = load i61, ptr @a, align 1
  %r = call i61 @llvm.fshr.i61(i61 %1, i61 %1, i61 1)
  ret i61 %r
}
