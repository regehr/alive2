@a = external global i43

declare i43 @llvm.fshr.i43 (i43 %a, i43 %b, i43 %c)

define i43 @f() {
  %1 = load i43, ptr @a, align 1
  %r = call i43 @llvm.fshr.i43(i43 %1, i43 %1, i43 1)
  ret i43 %r
}
