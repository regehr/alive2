@a = external global i14

declare i14 @llvm.fshr.i14 (i14 %a, i14 %b, i14 %c)

define void @f(i14 %arg) {
  %r = call i14 @llvm.fshr.i14(i14 %arg, i14 %arg, i14 1)
  store i14 %r, ptr @a, align 1
  ret void
}
