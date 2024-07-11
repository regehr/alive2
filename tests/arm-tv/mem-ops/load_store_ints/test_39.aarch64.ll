@a = external global i39

declare i39 @llvm.fshr.i39 (i39 %a, i39 %b, i39 %c)

define void @f() {
  %1 = load i39, ptr @a, align 1
  %r = call i39 @llvm.fshr.i39(i39 %1, i39 %1, i39 1)
  store i39 %r, ptr @a, align 1
  ret void
}
