@a = external global i54

declare i54 @llvm.fshr.i54 (i54 %a, i54 %b, i54 %c)

define i54 @f() {
  %1 = load i54, ptr @a, align 1
  %r = call i54 @llvm.fshr.i54(i54 %1, i54 %1, i54 1)
  ret i54 %r
}
