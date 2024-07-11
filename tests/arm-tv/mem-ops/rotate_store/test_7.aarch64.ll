@a = external global i7

declare i7 @llvm.fshr.i7 (i7 %a, i7 %b, i7 %c)

define void @f(i7 %arg) {
  %r = call i7 @llvm.fshr.i7(i7 %arg, i7 %arg, i7 1)
  store i7 %r, ptr @a, align 1
  ret void
}
