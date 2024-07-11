@a = external global i31

declare i31 @llvm.fshr.i31 (i31 %a, i31 %b, i31 %c)

define void @f(i31 %arg) {
  %r = call i31 @llvm.fshr.i31(i31 %arg, i31 %arg, i31 1)
  store i31 %r, ptr @a, align 1
  ret void
}
