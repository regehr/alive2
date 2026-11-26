; a compare and select against a splat, which llc lowers to vmin.vx
define void @test(ptr %d, ptr %a, i32 %k) {
  %x = load <4 x i32>, ptr %a, align 4
  %i = insertelement <4 x i32> poison, i32 %k, i64 0
  %s = shufflevector <4 x i32> %i, <4 x i32> poison, <4 x i32> zeroinitializer
  %c = icmp slt <4 x i32> %x, %s
  %r = select <4 x i1> %c, <4 x i32> %x, <4 x i32> %s
  store <4 x i32> %r, ptr %d, align 4
  ret void
}
