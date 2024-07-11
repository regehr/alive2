@a = external global i52

declare i52 @llvm.fshr.i52 (i52 %a, i52 %b, i52 %c)

define void @f(i52 %arg) {
  %r = call i52 @llvm.fshr.i52(i52 %arg, i52 %arg, i52 1)
  store i52 %r, ptr @a, align 1
  ret void
}
