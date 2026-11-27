; at e8, vrev8.v is the identity and vbrev8.v is vbrev.v
define void @test(ptr %d, ptr %e, ptr %a) {
  %x = load <16 x i8>, ptr %a
  store <16 x i8> %x, ptr %d
  %b = call <16 x i8> @llvm.bitreverse.v16i8(<16 x i8> %x)
  store <16 x i8> %b, ptr %e
  ret void
}
