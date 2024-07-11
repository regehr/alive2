@a = external global i46

declare i46 @llvm.fshr.i46 (i46 %a, i46 %b, i46 %c)

define void @f(i46 %arg) {
  %r = call i46 @llvm.fshr.i46(i46 %arg, i46 %arg, i46 1)
  store i46 %r, ptr @a, align 1
  ret void
}
