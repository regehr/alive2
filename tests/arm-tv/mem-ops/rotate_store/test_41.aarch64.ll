@a = external global i41

declare i41 @llvm.fshr.i41 (i41 %a, i41 %b, i41 %c)

define void @f(i41 %arg) {
  %r = call i41 @llvm.fshr.i41(i41 %arg, i41 %arg, i41 1)
  store i41 %r, ptr @a, align 1
  ret void
}
