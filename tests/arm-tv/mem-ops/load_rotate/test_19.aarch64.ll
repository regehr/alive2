@a = external global i19

declare i19 @llvm.fshr.i19 (i19 %a, i19 %b, i19 %c)

define i19 @f() {
  %1 = load i19, ptr @a, align 1
  %r = call i19 @llvm.fshr.i19(i19 %1, i19 %1, i19 1)
  ret i19 %r
}
