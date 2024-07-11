@a = external global i65

declare i65 @llvm.fshr.i65 (i65 %a, i65 %b, i65 %c)

define void @f() {
  %1 = load i65, ptr @a, align 1
  %r = call i65 @llvm.fshr.i65(i65 %1, i65 %1, i65 1)
  store i65 %r, ptr @a, align 1
  ret void
}
