@a = external global i71

declare i71 @llvm.fshr.i71 (i71 %a, i71 %b, i71 %c)

define void @f() {
  %1 = load i71, ptr @a, align 1
  %r = call i71 @llvm.fshr.i71(i71 %1, i71 %1, i71 1)
  store i71 %r, ptr @a, align 1
  ret void
}
