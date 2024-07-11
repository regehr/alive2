@a = external global i34

declare i34 @llvm.fshr.i34 (i34 %a, i34 %b, i34 %c)

define i34 @f() {
  %1 = load i34, ptr @a, align 1
  %r = call i34 @llvm.fshr.i34(i34 %1, i34 %1, i34 1)
  ret i34 %r
}
