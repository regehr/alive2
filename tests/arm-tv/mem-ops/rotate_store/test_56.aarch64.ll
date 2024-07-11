@a = external global i56

declare i56 @llvm.fshr.i56 (i56 %a, i56 %b, i56 %c)

define void @f(i56 %arg) {
  %r = call i56 @llvm.fshr.i56(i56 %arg, i56 %arg, i56 1)
  store i56 %r, ptr @a, align 1
  ret void
}
