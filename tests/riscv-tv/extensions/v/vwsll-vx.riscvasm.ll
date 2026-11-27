; vwsll.vx uses the low lg2(2 * SEW) bits of rs1
define void @test(ptr %d, ptr %a, i64 %amt) {
  %x = load <8 x i8>, ptr %a
  %z = zext <8 x i8> %x to <8 x i16>
  %t = trunc i64 %amt to i16
  %m = and i16 %t, 15
  %i = insertelement <8 x i16> poison, i16 %m, i64 0
  %s = shufflevector <8 x i16> %i, <8 x i16> poison, <8 x i32> zeroinitializer
  %r = shl <8 x i16> %z, %s
  store <8 x i16> %r, ptr %d
  ret void
}
