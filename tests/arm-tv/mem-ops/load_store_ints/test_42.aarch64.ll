@a = external global i42

declare i42 @llvm.fshr.i42 (i42 %a, i42 %b, i42 %c)

define void @f() {
  %1 = load i42, ptr @a, align 1
  %r = call i42 @llvm.fshr.i42(i42 %1, i42 %1, i42 1)
  store i42 %r, ptr @a, align 1
  ret void
}
