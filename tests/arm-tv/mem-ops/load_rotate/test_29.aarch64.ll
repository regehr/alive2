@a = external global i29

declare i29 @llvm.fshr.i29 (i29 %a, i29 %b, i29 %c)

define i29 @f() {
  %1 = load i29, ptr @a, align 1
  %r = call i29 @llvm.fshr.i29(i29 %1, i29 %1, i29 1)
  ret i29 %r
}
