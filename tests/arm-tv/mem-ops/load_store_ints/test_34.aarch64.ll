@a = external global i34

declare i34 @llvm.fshr.i34 (i34 %a, i34 %b, i34 %c)

define void @f() {
  %1 = load i34, ptr @a, align 1
  %r = call i34 @llvm.fshr.i34(i34 %1, i34 %1, i34 1)
  store i34 %r, ptr @a, align 1
  ret void
}
