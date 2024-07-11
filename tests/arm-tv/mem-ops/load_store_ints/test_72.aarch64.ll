@a = external global i72

declare i72 @llvm.fshr.i72 (i72 %a, i72 %b, i72 %c)

define void @f() {
  %1 = load i72, ptr @a, align 1
  %r = call i72 @llvm.fshr.i72(i72 %1, i72 %1, i72 1)
  store i72 %r, ptr @a, align 1
  ret void
}
