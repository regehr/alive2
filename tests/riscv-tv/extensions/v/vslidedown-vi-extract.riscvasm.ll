; extracting element 2 the way LLVM does: slide down, then vmv.x.s
define i64 @test(ptr %src) {
  %a0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %a1 = load i32, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %src, i64 8
  %a2 = load i32, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %src, i64 12
  %a3 = load i32, ptr %p3, align 4
  %r = sext i32 %a2 to i64
  ret i64 %r
}
