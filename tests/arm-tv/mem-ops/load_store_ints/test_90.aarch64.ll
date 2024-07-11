@a = external global i90

declare i90 @llvm.fshr.i90 (i90 %a, i90 %b, i90 %c)

define void @f() {
  %1 = load i90, ptr @a, align 1
  %r = call i90 @llvm.fshr.i90(i90 %1, i90 %1, i90 1)
  store i90 %r, ptr @a, align 1
  ret void
}
