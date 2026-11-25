; CHECK: 1 incorrect transformations
; the operands of vsub.vv are not in the scalar order: this is vs1 - vs2
define void @test(ptr %dst, ptr %a, ptr %b) {
  %a0 = load i8, ptr %a, align 1
  %b0 = load i8, ptr %b, align 1
  %pa1 = getelementptr i8, ptr %a, i64 1
  %pb1 = getelementptr i8, ptr %b, i64 1
  %a1 = load i8, ptr %pa1, align 1
  %b1 = load i8, ptr %pb1, align 1
  %r0 = sub i8 %b0, %a0
  %r1 = sub i8 %b1, %a1
  store i8 %r0, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %r1, ptr %p1, align 1
  ret void
}
