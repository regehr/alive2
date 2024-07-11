@a = external global i24

declare i24 @llvm.fshr.i24 (i24 %a, i24 %b, i24 %c)

define void @f(i24 %arg) {
  %r = call i24 @llvm.fshr.i24(i24 %arg, i24 %arg, i24 1)
  store i24 %r, ptr @a, align 1
  ret void
}
