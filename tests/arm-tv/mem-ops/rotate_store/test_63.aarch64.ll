@a = external global i63

declare i63 @llvm.fshr.i63 (i63 %a, i63 %b, i63 %c)

define void @f(i63 %arg) {
  %r = call i63 @llvm.fshr.i63(i63 %arg, i63 %arg, i63 1)
  store i63 %r, ptr @a, align 1
  ret void
}
