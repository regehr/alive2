@a = external global i70

declare i70 @llvm.fshr.i70 (i70 %a, i70 %b, i70 %c)

define void @f() {
  %1 = load i70, ptr @a, align 1
  %r = call i70 @llvm.fshr.i70(i70 %1, i70 %1, i70 1)
  store i70 %r, ptr @a, align 1
  ret void
}
