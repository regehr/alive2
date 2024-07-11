@a = external global i82

declare i82 @llvm.fshr.i82 (i82 %a, i82 %b, i82 %c)

define void @f() {
  %1 = load i82, ptr @a, align 1
  %r = call i82 @llvm.fshr.i82(i82 %1, i82 %1, i82 1)
  store i82 %r, ptr @a, align 1
  ret void
}
