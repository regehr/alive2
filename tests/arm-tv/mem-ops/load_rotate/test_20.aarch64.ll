@a = external global i20

declare i20 @llvm.fshr.i20 (i20 %a, i20 %b, i20 %c)

define i20 @f() {
  %1 = load i20, ptr @a, align 1
  %r = call i20 @llvm.fshr.i20(i20 %1, i20 %1, i20 1)
  ret i20 %r
}
