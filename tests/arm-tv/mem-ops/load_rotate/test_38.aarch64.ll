@a = external global i38

declare i38 @llvm.fshr.i38 (i38 %a, i38 %b, i38 %c)

define i38 @f() {
  %1 = load i38, ptr @a, align 1
  %r = call i38 @llvm.fshr.i38(i38 %1, i38 %1, i38 1)
  ret i38 %r
}
