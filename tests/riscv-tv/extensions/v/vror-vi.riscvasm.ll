; vror.vi uses only the low lg2(SEW) bits of its uimm6: 11 rotates
; an e8 element right by 3
define void @test(ptr %d, ptr %a) {
  %x = load <16 x i8>, ptr %a
  %r = call <16 x i8> @llvm.fshr.v16i8(<16 x i8> %x, <16 x i8> %x, <16 x i8> splat (i8 3))
  store <16 x i8> %r, ptr %d
  ret void
}
