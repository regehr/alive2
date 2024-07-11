@a = external global i16

declare i16 @llvm.fshr.i16 (i16 %a, i16 %b, i16 %c)

define i16 @f() {
  %1 = load i16, ptr @a, align 1
  %r = call i16 @llvm.fshr.i16(i16 %1, i16 %1, i16 1)
  ret i16 %r
}
