@a = external global i7

declare i7 @llvm.fshr.i7 (i7 %a, i7 %b, i7 %c)

define i7 @f() {
  %1 = load i7, ptr @a, align 1
  %r = call i7 @llvm.fshr.i7(i7 %1, i7 %1, i7 1)
  ret i7 %r
}
