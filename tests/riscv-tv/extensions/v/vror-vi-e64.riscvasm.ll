; vror.vi with an immediate of 32 or more, at e64
define void @test(ptr %d, ptr %a) {
  %x = load <2 x i64>, ptr %a
  %r = call <2 x i64> @llvm.fshr.v2i64(<2 x i64> %x, <2 x i64> %x, <2 x i64> splat (i64 45))
  store <2 x i64> %r, ptr %d
  ret void
}
