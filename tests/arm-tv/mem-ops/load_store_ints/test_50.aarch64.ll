@a = external global i50

declare i50 @llvm.fshr.i50 (i50 %a, i50 %b, i50 %c)

define void @f() {
  %1 = load i50, ptr @a, align 1
  %r = call i50 @llvm.fshr.i50(i50 %1, i50 %1, i50 1)
  store i50 %r, ptr @a, align 1
  ret void
}
