@a = external global i43

declare i43 @llvm.fshr.i43 (i43 %a, i43 %b, i43 %c)

define void @f(i43 %arg) {
  %r = call i43 @llvm.fshr.i43(i43 %arg, i43 %arg, i43 1)
  store i43 %r, ptr @a, align 1
  ret void
}
