; fixed-length vector code that llc lowers to RVV
define void @test(ptr %d, ptr %a, ptr %b) {
  %x = load <4 x i32>, ptr %a, align 4
  %y = load <4 x i32>, ptr %b, align 4
  %r = mul <4 x i32> %x, %y
  store <4 x i32> %r, ptr %d, align 4
  ret void
}
