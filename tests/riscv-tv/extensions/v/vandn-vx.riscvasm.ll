; vandn.vx: vs2 & ~(low SEW bits of rs1)
define void @test(ptr %d, ptr %a, i64 %y) {
  %x = load <8 x i16>, ptr %a
  %t = trunc i64 %y to i16
  %n = xor i16 %t, -1
  %i = insertelement <8 x i16> poison, i16 %n, i64 0
  %s = shufflevector <8 x i16> %i, <8 x i16> poison, <8 x i32> zeroinitializer
  %r = and <8 x i16> %x, %s
  store <8 x i16> %r, ptr %d
  ret void
}
