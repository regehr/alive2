@a = external global i16

declare i16 @llvm.fshr.i16 (i16 %a, i16 %b, i16 %c)

define void @f(i16 %arg) {
  %r = call i16 @llvm.fshr.i16(i16 %arg, i16 %arg, i16 1)
  store i16 %r, ptr @a, align 1
  ret void
}
