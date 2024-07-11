@a = external global i105

declare i105 @llvm.fshr.i105 (i105 %a, i105 %b, i105 %c)

define void @f() {
  %1 = load i105, ptr @a, align 1
  %r = call i105 @llvm.fshr.i105(i105 %1, i105 %1, i105 1)
  store i105 %r, ptr @a, align 1
  ret void
}
