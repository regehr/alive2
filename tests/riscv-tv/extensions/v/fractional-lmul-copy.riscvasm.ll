; with LMUL=1/2 and VLEN=128, VLMAX is two 32-bit elements
define void @test(ptr %dst, ptr %src) {
  %v = load i64, ptr %src, align 4
  store i64 %v, ptr %dst, align 4
  ret void
}
