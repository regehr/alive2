@a = external global i28

declare i28 @llvm.fshr.i28 (i28 %a, i28 %b, i28 %c)

define void @f(i28 %arg) {
  %r = call i28 @llvm.fshr.i28(i28 %arg, i28 %arg, i28 1)
  store i28 %r, ptr @a, align 1
  ret void
}
