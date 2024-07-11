@a = external global i59

declare i59 @llvm.fshr.i59 (i59 %a, i59 %b, i59 %c)

define void @f(i59 %arg) {
  %r = call i59 @llvm.fshr.i59(i59 %arg, i59 %arg, i59 1)
  store i59 %r, ptr @a, align 1
  ret void
}
