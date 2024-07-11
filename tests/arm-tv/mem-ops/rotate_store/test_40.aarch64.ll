@a = external global i40

declare i40 @llvm.fshr.i40 (i40 %a, i40 %b, i40 %c)

define void @f(i40 %arg) {
  %r = call i40 @llvm.fshr.i40(i40 %arg, i40 %arg, i40 1)
  store i40 %r, ptr @a, align 1
  ret void
}
