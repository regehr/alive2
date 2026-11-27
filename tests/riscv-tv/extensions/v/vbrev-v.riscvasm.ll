; vbrev.v reverses all the bits of each element
define void @test(ptr %d, ptr %a) {
  %x = load <2 x i64>, ptr %a
  %r = call <2 x i64> @llvm.bitreverse.v2i64(<2 x i64> %x)
  store <2 x i64> %r, ptr %d
  ret void
}
