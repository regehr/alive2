@a = external global i17

declare i17 @llvm.fshr.i17 (i17 %a, i17 %b, i17 %c)

define void @f() {
  %1 = load i17, ptr @a, align 1
  %r = call i17 @llvm.fshr.i17(i17 %1, i17 %1, i17 1)
  store i17 %r, ptr @a, align 1
  ret void
}
