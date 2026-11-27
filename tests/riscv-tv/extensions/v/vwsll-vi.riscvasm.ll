; vwsll.vi zero-extends to 2 * SEW and then shifts, so a shift amount
; larger than SEW keeps the high bits
define void @test(ptr %d, ptr %a) {
  %x = load <4 x i16>, ptr %a
  %z = zext <4 x i16> %x to <4 x i32>
  %r = shl <4 x i32> %z, splat (i32 20)
  store <4 x i32> %r, ptr %d
  ret void
}
