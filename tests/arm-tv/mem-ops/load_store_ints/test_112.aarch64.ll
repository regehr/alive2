@a = external global i112

declare i112 @llvm.fshr.i112 (i112 %a, i112 %b, i112 %c)

define void @f() {
  %1 = load i112, ptr @a, align 1
  %r = call i112 @llvm.fshr.i112(i112 %1, i112 %1, i112 1)
  store i112 %r, ptr @a, align 1
  ret void
}
