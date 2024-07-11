@a = external global i111

declare i111 @llvm.fshr.i111 (i111 %a, i111 %b, i111 %c)

define void @f() {
  %1 = load i111, ptr @a, align 1
  %r = call i111 @llvm.fshr.i111(i111 %1, i111 %1, i111 1)
  store i111 %r, ptr @a, align 1
  ret void
}
