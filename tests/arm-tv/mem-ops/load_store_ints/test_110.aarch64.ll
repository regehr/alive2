@a = external global i110

declare i110 @llvm.fshr.i110 (i110 %a, i110 %b, i110 %c)

define void @f() {
  %1 = load i110, ptr @a, align 1
  %r = call i110 @llvm.fshr.i110(i110 %1, i110 %1, i110 1)
  store i110 %r, ptr @a, align 1
  ret void
}
