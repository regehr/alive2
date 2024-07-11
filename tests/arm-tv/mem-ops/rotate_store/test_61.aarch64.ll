@a = external global i61

declare i61 @llvm.fshr.i61 (i61 %a, i61 %b, i61 %c)

define void @f(i61 %arg) {
  %r = call i61 @llvm.fshr.i61(i61 %arg, i61 %arg, i61 1)
  store i61 %r, ptr @a, align 1
  ret void
}
