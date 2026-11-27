; vror.vv: per-element rotate right, amount taken mod SEW
define void @test(ptr %d, ptr %a, ptr %b) {
  %x = load <4 x i32>, ptr %a
  %y = load <4 x i32>, ptr %b
  %r = call <4 x i32> @llvm.fshr.v4i32(<4 x i32> %x, <4 x i32> %x, <4 x i32> %y)
  store <4 x i32> %r, ptr %d
  ret void
}
