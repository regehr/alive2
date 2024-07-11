@a = external global i40

declare i40 @llvm.fshr.i40 (i40 %a, i40 %b, i40 %c)

define i40 @f() {
  %1 = load i40, ptr @a, align 1
  %r = call i40 @llvm.fshr.i40(i40 %1, i40 %1, i40 1)
  ret i40 %r
}
