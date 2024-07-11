@a = external global i116

declare i116 @llvm.fshr.i116 (i116 %a, i116 %b, i116 %c)

define void @f() {
  %1 = load i116, ptr @a, align 1
  %r = call i116 @llvm.fshr.i116(i116 %1, i116 %1, i116 1)
  store i116 %r, ptr @a, align 1
  ret void
}
