@a = external global i23

declare i23 @llvm.fshr.i23 (i23 %a, i23 %b, i23 %c)

define i23 @f() {
  %1 = load i23, ptr @a, align 1
  %r = call i23 @llvm.fshr.i23(i23 %1, i23 %1, i23 1)
  ret i23 %r
}
