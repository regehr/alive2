@a = external global i2

declare i2 @llvm.fshr.i2 (i2 %a, i2 %b, i2 %c)

define void @f() {
  %1 = load i2, ptr @a, align 1
  %r = call i2 @llvm.fshr.i2(i2 %1, i2 %1, i2 1)
  store i2 %r, ptr @a, align 1
  ret void
}
