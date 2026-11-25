; vsub.vv vd, vs2, vs1 computes vs2 - vs1
define void @test(ptr %dst, ptr %a, ptr %b) {
  %a0 = load i16, ptr %a, align 2
  %b0 = load i16, ptr %b, align 2
  %pa1 = getelementptr i8, ptr %a, i64 2
  %pb1 = getelementptr i8, ptr %b, i64 2
  %a1 = load i16, ptr %pa1, align 2
  %b1 = load i16, ptr %pb1, align 2
  %r0 = sub i16 %a0, %b0
  %r1 = sub i16 %a1, %b1
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
