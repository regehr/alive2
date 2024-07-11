@a = external global i66

declare i66 @llvm.fshr.i66 (i66 %a, i66 %b, i66 %c)

define void @f() {
  %1 = load i66, ptr @a, align 1
  %r = call i66 @llvm.fshr.i66(i66 %1, i66 %1, i66 1)
  store i66 %r, ptr @a, align 1
  ret void
}
