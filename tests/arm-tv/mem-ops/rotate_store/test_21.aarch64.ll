@a = external global i21

declare i21 @llvm.fshr.i21 (i21 %a, i21 %b, i21 %c)

define void @f(i21 %arg) {
  %r = call i21 @llvm.fshr.i21(i21 %arg, i21 %arg, i21 1)
  store i21 %r, ptr @a, align 1
  ret void
}
