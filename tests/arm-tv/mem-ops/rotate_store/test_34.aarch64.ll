@a = external global i34

declare i34 @llvm.fshr.i34 (i34 %a, i34 %b, i34 %c)

define void @f(i34 %arg) {
  %r = call i34 @llvm.fshr.i34(i34 %arg, i34 %arg, i34 1)
  store i34 %r, ptr @a, align 1
  ret void
}
