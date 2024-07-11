@a = external global i78

declare i78 @llvm.fshr.i78 (i78 %a, i78 %b, i78 %c)

define void @f() {
  %1 = load i78, ptr @a, align 1
  %r = call i78 @llvm.fshr.i78(i78 %1, i78 %1, i78 1)
  store i78 %r, ptr @a, align 1
  ret void
}
