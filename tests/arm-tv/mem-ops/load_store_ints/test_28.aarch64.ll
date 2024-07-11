@a = external global i28

declare i28 @llvm.fshr.i28 (i28 %a, i28 %b, i28 %c)

define void @f() {
  %1 = load i28, ptr @a, align 1
  %r = call i28 @llvm.fshr.i28(i28 %1, i28 %1, i28 1)
  store i28 %r, ptr @a, align 1
  ret void
}
