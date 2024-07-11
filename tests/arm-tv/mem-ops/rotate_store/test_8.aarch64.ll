@a = external global i8

declare i8 @llvm.fshr.i8 (i8 %a, i8 %b, i8 %c)

define void @f(i8 %arg) {
  %r = call i8 @llvm.fshr.i8(i8 %arg, i8 %arg, i8 1)
  store i8 %r, ptr @a, align 1
  ret void
}
