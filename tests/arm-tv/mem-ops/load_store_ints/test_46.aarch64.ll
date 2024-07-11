@a = external global i46

declare i46 @llvm.fshr.i46 (i46 %a, i46 %b, i46 %c)

define void @f() {
  %1 = load i46, ptr @a, align 1
  %r = call i46 @llvm.fshr.i46(i46 %1, i46 %1, i46 1)
  store i46 %r, ptr @a, align 1
  ret void
}
