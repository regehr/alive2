@a = external global i81

declare i81 @llvm.fshr.i81 (i81 %a, i81 %b, i81 %c)

define void @f() {
  %1 = load i81, ptr @a, align 1
  %r = call i81 @llvm.fshr.i81(i81 %1, i81 %1, i81 1)
  store i81 %r, ptr @a, align 1
  ret void
}
