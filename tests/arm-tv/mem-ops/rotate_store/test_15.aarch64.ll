@a = external global i15

declare i15 @llvm.fshr.i15 (i15 %a, i15 %b, i15 %c)

define void @f(i15 %arg) {
  %r = call i15 @llvm.fshr.i15(i15 %arg, i15 %arg, i15 1)
  store i15 %r, ptr @a, align 1
  ret void
}
