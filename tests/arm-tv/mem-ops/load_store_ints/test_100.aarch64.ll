@a = external global i100

declare i100 @llvm.fshr.i100 (i100 %a, i100 %b, i100 %c)

define void @f() {
  %1 = load i100, ptr @a, align 1
  %r = call i100 @llvm.fshr.i100(i100 %1, i100 %1, i100 1)
  store i100 %r, ptr @a, align 1
  ret void
}
