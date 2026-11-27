; vrol.vx: rotate left by the low lg2(SEW) bits of rs1
define void @test(ptr %d, ptr %a, i64 %amt) {
  %x = load <4 x i32>, ptr %a
  %t = trunc i64 %amt to i32
  %i = insertelement <4 x i32> poison, i32 %t, i64 0
  %s = shufflevector <4 x i32> %i, <4 x i32> poison, <4 x i32> zeroinitializer
  %r = call <4 x i32> @llvm.fshl.v4i32(<4 x i32> %x, <4 x i32> %x, <4 x i32> %s)
  store <4 x i32> %r, ptr %d
  ret void
}
