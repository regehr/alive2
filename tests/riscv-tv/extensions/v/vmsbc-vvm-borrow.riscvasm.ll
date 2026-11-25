; the borrow out of a - b - bin is 1 iff the untruncated difference is
; negative; bin is bit 0 of %m
define void @test(ptr %dst, ptr %pa, ptr %pb, i64 %m) {
  %a = load i8, ptr %pa, align 1
  %b = load i8, ptr %pb, align 1
  %wa = zext i8 %a to i16
  %wb = zext i8 %b to i16
  %bin = and i64 %m, 1
  %bin16 = trunc i64 %bin to i16
  %sub = add i16 %wb, %bin16
  %borrow = icmp ult i16 %wa, %sub
  %r = zext i1 %borrow to i8
  store i8 %r, ptr %dst, align 1
  ret void
}
