@a = external global i94

declare i94 @llvm.fshr.i94 (i94 %a, i94 %b, i94 %c)

define void @f() {
  %1 = load i94, ptr @a, align 1
  %r = call i94 @llvm.fshr.i94(i94 %1, i94 %1, i94 1)
  store i94 %r, ptr @a, align 1
  ret void
}
