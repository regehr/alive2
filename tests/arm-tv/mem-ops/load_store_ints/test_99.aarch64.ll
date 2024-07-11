@a = external global i99

declare i99 @llvm.fshr.i99 (i99 %a, i99 %b, i99 %c)

define void @f() {
  %1 = load i99, ptr @a, align 1
  %r = call i99 @llvm.fshr.i99(i99 %1, i99 %1, i99 1)
  store i99 %r, ptr @a, align 1
  ret void
}
