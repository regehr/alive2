@a = external global i69

declare i69 @llvm.fshr.i69 (i69 %a, i69 %b, i69 %c)

define void @f() {
  %1 = load i69, ptr @a, align 1
  %r = call i69 @llvm.fshr.i69(i69 %1, i69 %1, i69 1)
  store i69 %r, ptr @a, align 1
  ret void
}
