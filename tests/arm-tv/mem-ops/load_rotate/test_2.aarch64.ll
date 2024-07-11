@a = external global i2

declare i2 @llvm.fshr.i2 (i2 %a, i2 %b, i2 %c)

define i2 @f() {
  %1 = load i2, ptr @a, align 1
  %r = call i2 @llvm.fshr.i2(i2 %1, i2 %1, i2 1)
  ret i2 %r
}
