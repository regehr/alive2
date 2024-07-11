@a = external global i108

declare i108 @llvm.fshr.i108 (i108 %a, i108 %b, i108 %c)

define void @f() {
  %1 = load i108, ptr @a, align 1
  %r = call i108 @llvm.fshr.i108(i108 %1, i108 %1, i108 1)
  store i108 %r, ptr @a, align 1
  ret void
}
