@a = external global i20

declare i20 @llvm.fshr.i20 (i20 %a, i20 %b, i20 %c)

define void @f(i20 %arg) {
  %r = call i20 @llvm.fshr.i20(i20 %arg, i20 %arg, i20 1)
  store i20 %r, ptr @a, align 1
  ret void
}
