; vmv.v.x uses the low SEW bits of the scalar
define void @test(ptr %dst, i64 %x) {
  %t = trunc i64 %x to i16
  store i16 %t, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %t, ptr %p1, align 2
  ret void
}
