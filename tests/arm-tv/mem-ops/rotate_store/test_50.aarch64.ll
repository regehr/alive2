@a = external global i50

declare i50 @llvm.fshr.i50 (i50 %a, i50 %b, i50 %c)

define void @f(i50 %arg) {
  %r = call i50 @llvm.fshr.i50(i50 %arg, i50 %arg, i50 1)
  store i50 %r, ptr @a, align 1
  ret void
}
