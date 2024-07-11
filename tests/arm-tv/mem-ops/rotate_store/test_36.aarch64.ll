@a = external global i36

declare i36 @llvm.fshr.i36 (i36 %a, i36 %b, i36 %c)

define void @f(i36 %arg) {
  %r = call i36 @llvm.fshr.i36(i36 %arg, i36 %arg, i36 1)
  store i36 %r, ptr @a, align 1
  ret void
}
