@a = external global i107

declare i107 @llvm.fshr.i107 (i107 %a, i107 %b, i107 %c)

define void @f() {
  %1 = load i107, ptr @a, align 1
  %r = call i107 @llvm.fshr.i107(i107 %1, i107 %1, i107 1)
  store i107 %r, ptr @a, align 1
  ret void
}
