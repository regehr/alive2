@a = external global i73

declare i73 @llvm.fshr.i73 (i73 %a, i73 %b, i73 %c)

define void @f() {
  %1 = load i73, ptr @a, align 1
  %r = call i73 @llvm.fshr.i73(i73 %1, i73 %1, i73 1)
  store i73 %r, ptr @a, align 1
  ret void
}
