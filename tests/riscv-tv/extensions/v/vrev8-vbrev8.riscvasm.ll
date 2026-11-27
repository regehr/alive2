; vrev8.v swaps the bytes of each element; vbrev8.v reverses the bits
; within each byte
define void @test(ptr %d, ptr %e, ptr %a) {
  %x = load <4 x i32>, ptr %a
  %r = call <4 x i32> @llvm.bswap.v4i32(<4 x i32> %x)
  store <4 x i32> %r, ptr %d
  %b = call <4 x i32> @llvm.bitreverse.v4i32(<4 x i32> %r)
  store <4 x i32> %b, ptr %e
  ret void
}
