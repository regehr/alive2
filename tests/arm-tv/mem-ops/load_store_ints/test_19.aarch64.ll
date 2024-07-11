@a = external global i19

declare i19 @llvm.fshr.i19 (i19 %a, i19 %b, i19 %c)

define void @f() {
  %1 = load i19, ptr @a, align 1
  %r = call i19 @llvm.fshr.i19(i19 %1, i19 %1, i19 1)
  store i19 %r, ptr @a, align 1
  ret void
}
