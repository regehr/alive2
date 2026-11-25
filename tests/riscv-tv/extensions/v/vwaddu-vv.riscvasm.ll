; vwaddu.vv zero-extends both SEW sources and writes a 2*SEW sum
define void @test(ptr %dst, ptr %pa, ptr %pb) {
  %a0 = load i8, ptr %pa, align 1
  %b0 = load i8, ptr %pb, align 1
  %qa = getelementptr i8, ptr %pa, i64 1
  %qb = getelementptr i8, ptr %pb, i64 1
  %a1 = load i8, ptr %qa, align 1
  %b1 = load i8, ptr %qb, align 1
  %wa0 = zext i8 %a0 to i16
  %wb0 = zext i8 %b0 to i16
  %wa1 = zext i8 %a1 to i16
  %wb1 = zext i8 %b1 to i16
  %r0 = add i16 %wa0, %wb0
  %r1 = add i16 %wa1, %wb1
  store i16 %r0, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %r1, ptr %p1, align 2
  ret void
}
