@a = external global i51

declare i51 @llvm.fshr.i51 (i51 %a, i51 %b, i51 %c)

define void @f() {
  %1 = load i51, ptr @a, align 1
  %r = call i51 @llvm.fshr.i51(i51 %1, i51 %1, i51 1)
  store i51 %r, ptr @a, align 1
  ret void
}
