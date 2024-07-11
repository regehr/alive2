@a = external global i84

declare i84 @llvm.fshr.i84 (i84 %a, i84 %b, i84 %c)

define void @f() {
  %1 = load i84, ptr @a, align 1
  %r = call i84 @llvm.fshr.i84(i84 %1, i84 %1, i84 1)
  store i84 %r, ptr @a, align 1
  ret void
}
