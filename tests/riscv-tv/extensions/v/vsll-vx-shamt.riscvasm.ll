; only the low lg2(SEW) bits of the shift amount are used
define void @test(ptr %dst, i64 %x) {
  %t = trunc i64 %x to i8
  %m = and i8 %t, 7
  %s = shl i8 1, %m
  store i8 %s, ptr %dst, align 1
  %p1 = getelementptr i8, ptr %dst, i64 1
  store i8 %s, ptr %p1, align 1
  ret void
}
