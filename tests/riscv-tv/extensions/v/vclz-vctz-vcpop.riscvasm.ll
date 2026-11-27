; vclz.v and vctz.v of zero are SEW
define void @test(ptr %d, ptr %e, ptr %f, ptr %a) {
  %x = load <8 x i16>, ptr %a
  %l = call <8 x i16> @llvm.ctlz.v8i16(<8 x i16> %x, i1 false)
  store <8 x i16> %l, ptr %d
  %t = call <8 x i16> @llvm.cttz.v8i16(<8 x i16> %x, i1 false)
  store <8 x i16> %t, ptr %e
  %p = call <8 x i16> @llvm.ctpop.v8i16(<8 x i16> %x)
  store <8 x i16> %p, ptr %f
  ret void
}
