@a = external global i33

declare i33 @llvm.fshr.i33 (i33 %a, i33 %b, i33 %c)

define void @f(i33 %arg) {
  %r = call i33 @llvm.fshr.i33(i33 %arg, i33 %arg, i33 1)
  store i33 %r, ptr @a, align 1
  ret void
}
