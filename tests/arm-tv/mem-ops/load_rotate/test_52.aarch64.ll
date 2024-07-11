@a = external global i52

declare i52 @llvm.fshr.i52 (i52 %a, i52 %b, i52 %c)

define i52 @f() {
  %1 = load i52, ptr @a, align 1
  %r = call i52 @llvm.fshr.i52(i52 %1, i52 %1, i52 1)
  ret i52 %r
}
