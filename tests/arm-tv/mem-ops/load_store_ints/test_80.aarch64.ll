@a = external global i80

declare i80 @llvm.fshr.i80 (i80 %a, i80 %b, i80 %c)

define void @f() {
  %1 = load i80, ptr @a, align 1
  %r = call i80 @llvm.fshr.i80(i80 %1, i80 %1, i80 1)
  store i80 %r, ptr @a, align 1
  ret void
}
