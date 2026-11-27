; vwsll.vv: the narrow shift amounts are zero-extended
define void @test(ptr %d, ptr %a, ptr %b) {
  %x = load <4 x i16>, ptr %a
  %y = load <4 x i16>, ptr %b
  %zx = zext <4 x i16> %x to <4 x i32>
  %zy = zext <4 x i16> %y to <4 x i32>
  %m = and <4 x i32> %zy, splat (i32 31)
  %r = shl <4 x i32> %zx, %m
  store <4 x i32> %r, ptr %d
  ret void
}
