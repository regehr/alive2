@a = external global i114

declare i114 @llvm.fshr.i114 (i114 %a, i114 %b, i114 %c)

define void @f() {
  %1 = load i114, ptr @a, align 1
  %r = call i114 @llvm.fshr.i114(i114 %1, i114 %1, i114 1)
  store i114 %r, ptr @a, align 1
  ret void
}
