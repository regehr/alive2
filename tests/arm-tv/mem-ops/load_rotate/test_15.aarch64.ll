@a = external global i15

declare i15 @llvm.fshr.i15 (i15 %a, i15 %b, i15 %c)

define i15 @f() {
  %1 = load i15, ptr @a, align 1
  %r = call i15 @llvm.fshr.i15(i15 %1, i15 %1, i15 1)
  ret i15 %r
}
