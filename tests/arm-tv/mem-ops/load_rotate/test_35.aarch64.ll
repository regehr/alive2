@a = external global i35

declare i35 @llvm.fshr.i35 (i35 %a, i35 %b, i35 %c)

define i35 @f() {
  %1 = load i35, ptr @a, align 1
  %r = call i35 @llvm.fshr.i35(i35 %1, i35 %1, i35 1)
  ret i35 %r
}
