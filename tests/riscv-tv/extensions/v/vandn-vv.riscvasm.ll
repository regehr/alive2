; vandn.vv: vs2 & ~vs1
define void @test(ptr %d, ptr %a, ptr %b) {
  %x = load <4 x i32>, ptr %a
  %y = load <4 x i32>, ptr %b
  %n = xor <4 x i32> %y, splat (i32 -1)
  %r = and <4 x i32> %x, %n
  store <4 x i32> %r, ptr %d
  ret void
}
