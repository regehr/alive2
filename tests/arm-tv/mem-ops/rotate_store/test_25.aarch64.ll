@a = external global i25

declare i25 @llvm.fshr.i25 (i25 %a, i25 %b, i25 %c)

define void @f(i25 %arg) {
  %r = call i25 @llvm.fshr.i25(i25 %arg, i25 %arg, i25 1)
  store i25 %r, ptr @a, align 1
  ret void
}
