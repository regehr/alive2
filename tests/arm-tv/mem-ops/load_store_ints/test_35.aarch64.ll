@a = external global i35

declare i35 @llvm.fshr.i35 (i35 %a, i35 %b, i35 %c)

define void @f() {
  %1 = load i35, ptr @a, align 1
  %r = call i35 @llvm.fshr.i35(i35 %1, i35 %1, i35 1)
  store i35 %r, ptr @a, align 1
  ret void
}
