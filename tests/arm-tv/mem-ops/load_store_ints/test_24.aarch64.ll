@a = external global i24

declare i24 @llvm.fshr.i24 (i24 %a, i24 %b, i24 %c)

define void @f() {
  %1 = load i24, ptr @a, align 1
  %r = call i24 @llvm.fshr.i24(i24 %1, i24 %1, i24 1)
  store i24 %r, ptr @a, align 1
  ret void
}
