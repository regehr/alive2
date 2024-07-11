@a = external global i48

declare i48 @llvm.fshr.i48 (i48 %a, i48 %b, i48 %c)

define void @f(i48 %arg) {
  %r = call i48 @llvm.fshr.i48(i48 %arg, i48 %arg, i48 1)
  store i48 %r, ptr @a, align 1
  ret void
}
