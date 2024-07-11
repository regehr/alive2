@a = external global i35

declare i35 @llvm.fshr.i35 (i35 %a, i35 %b, i35 %c)

define void @f(i35 %arg) {
  %r = call i35 @llvm.fshr.i35(i35 %arg, i35 %arg, i35 1)
  store i35 %r, ptr @a, align 1
  ret void
}
