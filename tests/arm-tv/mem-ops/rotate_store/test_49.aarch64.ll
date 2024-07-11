@a = external global i49

declare i49 @llvm.fshr.i49 (i49 %a, i49 %b, i49 %c)

define void @f(i49 %arg) {
  %r = call i49 @llvm.fshr.i49(i49 %arg, i49 %arg, i49 1)
  store i49 %r, ptr @a, align 1
  ret void
}
