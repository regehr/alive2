@a = external global i92

declare i92 @llvm.fshr.i92 (i92 %a, i92 %b, i92 %c)

define void @f() {
  %1 = load i92, ptr @a, align 1
  %r = call i92 @llvm.fshr.i92(i92 %1, i92 %1, i92 1)
  store i92 %r, ptr @a, align 1
  ret void
}
