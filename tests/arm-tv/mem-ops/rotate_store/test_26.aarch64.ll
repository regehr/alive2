@a = external global i26

declare i26 @llvm.fshr.i26 (i26 %a, i26 %b, i26 %c)

define void @f(i26 %arg) {
  %r = call i26 @llvm.fshr.i26(i26 %arg, i26 %arg, i26 1)
  store i26 %r, ptr @a, align 1
  ret void
}
