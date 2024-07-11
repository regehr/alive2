@a = external global i49

declare i49 @llvm.fshr.i49 (i49 %a, i49 %b, i49 %c)

define void @f() {
  %1 = load i49, ptr @a, align 1
  %r = call i49 @llvm.fshr.i49(i49 %1, i49 %1, i49 1)
  store i49 %r, ptr @a, align 1
  ret void
}
