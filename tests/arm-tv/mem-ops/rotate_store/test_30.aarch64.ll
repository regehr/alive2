@a = external global i30

declare i30 @llvm.fshr.i30 (i30 %a, i30 %b, i30 %c)

define void @f(i30 %arg) {
  %r = call i30 @llvm.fshr.i30(i30 %arg, i30 %arg, i30 1)
  store i30 %r, ptr @a, align 1
  ret void
}
