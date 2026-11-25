; vadd.vx adds the low SEW bits of the scalar to each element
define void @test(ptr %dst, i64 %x) {
  %t = trunc i64 %x to i16
  %s = add i16 %t, 5
  store i16 %s, ptr %dst, align 2
  %p1 = getelementptr i8, ptr %dst, i64 2
  store i16 %s, ptr %p1, align 2
  ret void
}
