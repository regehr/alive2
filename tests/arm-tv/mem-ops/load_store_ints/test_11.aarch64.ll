@a = external global i11

declare i11 @llvm.fshr.i11 (i11 %a, i11 %b, i11 %c)

define void @f() {
  %1 = load i11, ptr @a, align 1
  %r = call i11 @llvm.fshr.i11(i11 %1, i11 %1, i11 1)
  store i11 %r, ptr @a, align 1
  ret void
}
