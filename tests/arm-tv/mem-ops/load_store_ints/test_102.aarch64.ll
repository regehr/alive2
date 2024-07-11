@a = external global i102

declare i102 @llvm.fshr.i102 (i102 %a, i102 %b, i102 %c)

define void @f() {
  %1 = load i102, ptr @a, align 1
  %r = call i102 @llvm.fshr.i102(i102 %1, i102 %1, i102 1)
  store i102 %r, ptr @a, align 1
  ret void
}
