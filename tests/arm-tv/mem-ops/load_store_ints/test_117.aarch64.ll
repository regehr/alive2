@a = external global i117

declare i117 @llvm.fshr.i117 (i117 %a, i117 %b, i117 %c)

define void @f() {
  %1 = load i117, ptr @a, align 1
  %r = call i117 @llvm.fshr.i117(i117 %1, i117 %1, i117 1)
  store i117 %r, ptr @a, align 1
  ret void
}
