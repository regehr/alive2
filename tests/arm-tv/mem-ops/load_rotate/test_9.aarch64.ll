@a = external global i9

declare i9 @llvm.fshr.i9 (i9 %a, i9 %b, i9 %c)

define i9 @f() {
  %1 = load i9, ptr @a, align 1
  %r = call i9 @llvm.fshr.i9(i9 %1, i9 %1, i9 1)
  ret i9 %r
}
