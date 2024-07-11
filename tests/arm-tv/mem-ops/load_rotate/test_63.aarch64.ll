@a = external global i63

declare i63 @llvm.fshr.i63 (i63 %a, i63 %b, i63 %c)

define i63 @f() {
  %1 = load i63, ptr @a, align 1
  %r = call i63 @llvm.fshr.i63(i63 %1, i63 %1, i63 1)
  ret i63 %r
}
