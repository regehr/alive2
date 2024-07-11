@a = external global i101

declare i101 @llvm.fshr.i101 (i101 %a, i101 %b, i101 %c)

define void @f() {
  %1 = load i101, ptr @a, align 1
  %r = call i101 @llvm.fshr.i101(i101 %1, i101 %1, i101 1)
  store i101 %r, ptr @a, align 1
  ret void
}
