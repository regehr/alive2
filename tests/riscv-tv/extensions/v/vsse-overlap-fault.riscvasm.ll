; CHECK: 1 incorrect transformations
; the element stores of vsse are unordered, so with stride 0 the value
; left in memory is not determined. the lifter conservatively treats a
; strided store whose active elements may overlap as a fault
define void @test(ptr %dst, ptr %src) {
  %a0 = load i32, ptr %src, align 4
  %p1 = getelementptr i8, ptr %src, i64 4
  %a1 = load i32, ptr %p1, align 4
  store i32 %a1, ptr %dst, align 4
  ret void
}
