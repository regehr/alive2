@a = external global i60

declare i60 @llvm.fshr.i60 (i60 %a, i60 %b, i60 %c)

define void @f() {
  %1 = load i60, ptr @a, align 1
  %r = call i60 @llvm.fshr.i60(i60 %1, i60 %1, i60 1)
  store i60 %r, ptr @a, align 1
  ret void
}
