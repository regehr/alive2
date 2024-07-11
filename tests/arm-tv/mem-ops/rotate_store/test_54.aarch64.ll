@a = external global i54

declare i54 @llvm.fshr.i54 (i54 %a, i54 %b, i54 %c)

define void @f(i54 %arg) {
  %r = call i54 @llvm.fshr.i54(i54 %arg, i54 %arg, i54 1)
  store i54 %r, ptr @a, align 1
  ret void
}
