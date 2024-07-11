@a = external global i12

declare i12 @llvm.fshr.i12 (i12 %a, i12 %b, i12 %c)

define void @f(i12 %arg) {
  %r = call i12 @llvm.fshr.i12(i12 %arg, i12 %arg, i12 1)
  store i12 %r, ptr @a, align 1
  ret void
}
