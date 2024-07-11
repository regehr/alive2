@a = external global i115

declare i115 @llvm.fshr.i115 (i115 %a, i115 %b, i115 %c)

define void @f() {
  %1 = load i115, ptr @a, align 1
  %r = call i115 @llvm.fshr.i115(i115 %1, i115 %1, i115 1)
  store i115 %r, ptr @a, align 1
  ret void
}
