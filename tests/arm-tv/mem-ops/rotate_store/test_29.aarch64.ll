@a = external global i29

declare i29 @llvm.fshr.i29 (i29 %a, i29 %b, i29 %c)

define void @f(i29 %arg) {
  %r = call i29 @llvm.fshr.i29(i29 %arg, i29 %arg, i29 1)
  store i29 %r, ptr @a, align 1
  ret void
}
