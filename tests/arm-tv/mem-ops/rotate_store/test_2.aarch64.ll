@a = external global i2

declare i2 @llvm.fshr.i2 (i2 %a, i2 %b, i2 %c)

define void @f(i2 %arg) {
  %r = call i2 @llvm.fshr.i2(i2 %arg, i2 %arg, i2 1)
  store i2 %r, ptr @a, align 1
  ret void
}
