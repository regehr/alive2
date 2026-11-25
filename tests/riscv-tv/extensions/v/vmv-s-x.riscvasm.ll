; vmv.s.x writes the low SEW bits of the scalar to element 0
define void @test(ptr %dst, i64 %x) {
  %t = trunc i64 %x to i32
  store i32 %t, ptr %dst, align 4
  ret void
}
