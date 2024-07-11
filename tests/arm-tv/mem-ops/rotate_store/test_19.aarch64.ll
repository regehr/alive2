@a = external global i19

declare i19 @llvm.fshr.i19 (i19 %a, i19 %b, i19 %c)

define void @f(i19 %arg) {
  %r = call i19 @llvm.fshr.i19(i19 %arg, i19 %arg, i19 1)
  store i19 %r, ptr @a, align 1
  ret void
}
