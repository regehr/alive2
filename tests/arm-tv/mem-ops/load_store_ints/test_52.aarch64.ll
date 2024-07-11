@a = external global i52

declare i52 @llvm.fshr.i52 (i52 %a, i52 %b, i52 %c)

define void @f() {
  %1 = load i52, ptr @a, align 1
  %r = call i52 @llvm.fshr.i52(i52 %1, i52 %1, i52 1)
  store i52 %r, ptr @a, align 1
  ret void
}
