@a = external global i96

declare i96 @llvm.fshr.i96 (i96 %a, i96 %b, i96 %c)

define void @f() {
  %1 = load i96, ptr @a, align 1
  %r = call i96 @llvm.fshr.i96(i96 %1, i96 %1, i96 1)
  store i96 %r, ptr @a, align 1
  ret void
}
