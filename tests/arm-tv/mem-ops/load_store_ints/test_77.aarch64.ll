@a = external global i77

declare i77 @llvm.fshr.i77 (i77 %a, i77 %b, i77 %c)

define void @f() {
  %1 = load i77, ptr @a, align 1
  %r = call i77 @llvm.fshr.i77(i77 %1, i77 %1, i77 1)
  store i77 %r, ptr @a, align 1
  ret void
}
