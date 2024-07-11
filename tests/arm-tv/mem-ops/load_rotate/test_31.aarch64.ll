@a = external global i31

declare i31 @llvm.fshr.i31 (i31 %a, i31 %b, i31 %c)

define i31 @f() {
  %1 = load i31, ptr @a, align 1
  %r = call i31 @llvm.fshr.i31(i31 %1, i31 %1, i31 1)
  ret i31 %r
}
