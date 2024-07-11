@a = external global i104

declare i104 @llvm.fshr.i104 (i104 %a, i104 %b, i104 %c)

define void @f() {
  %1 = load i104, ptr @a, align 1
  %r = call i104 @llvm.fshr.i104(i104 %1, i104 %1, i104 1)
  store i104 %r, ptr @a, align 1
  ret void
}
