@a = external global i57

declare i57 @llvm.fshr.i57 (i57 %a, i57 %b, i57 %c)

define void @f(i57 %arg) {
  %r = call i57 @llvm.fshr.i57(i57 %arg, i57 %arg, i57 1)
  store i57 %r, ptr @a, align 1
  ret void
}
