@a = external global i38

declare i38 @llvm.fshr.i38 (i38 %a, i38 %b, i38 %c)

define void @f() {
  %1 = load i38, ptr @a, align 1
  %r = call i38 @llvm.fshr.i38(i38 %1, i38 %1, i38 1)
  store i38 %r, ptr @a, align 1
  ret void
}
