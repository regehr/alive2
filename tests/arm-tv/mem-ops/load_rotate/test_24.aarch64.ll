@a = external global i24

declare i24 @llvm.fshr.i24 (i24 %a, i24 %b, i24 %c)

define i24 @f() {
  %1 = load i24, ptr @a, align 1
  %r = call i24 @llvm.fshr.i24(i24 %1, i24 %1, i24 1)
  ret i24 %r
}
