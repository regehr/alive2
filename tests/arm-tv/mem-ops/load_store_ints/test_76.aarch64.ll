@a = external global i76

declare i76 @llvm.fshr.i76 (i76 %a, i76 %b, i76 %c)

define void @f() {
  %1 = load i76, ptr @a, align 1
  %r = call i76 @llvm.fshr.i76(i76 %1, i76 %1, i76 1)
  store i76 %r, ptr @a, align 1
  ret void
}
