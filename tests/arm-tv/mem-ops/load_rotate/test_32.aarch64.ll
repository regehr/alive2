@a = external global i32

declare i32 @llvm.fshr.i32 (i32 %a, i32 %b, i32 %c)

define i32 @f() {
  %1 = load i32, ptr @a, align 1
  %r = call i32 @llvm.fshr.i32(i32 %1, i32 %1, i32 1)
  ret i32 %r
}