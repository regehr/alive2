@a = external global i13

declare i13 @llvm.fshr.i13 (i13 %a, i13 %b, i13 %c)

define void @f() {
  %1 = load i13, ptr @a, align 1
  %r = call i13 @llvm.fshr.i13(i13 %1, i13 %1, i13 1)
  store i13 %r, ptr @a, align 1
  ret void
}
