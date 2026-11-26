; vector arguments don't use up GPRs: x is still in a0
define i64 @test(i64 %x, <vscale x 1 x i64> %v) {
  %e = extractelement <vscale x 1 x i64> %v, i64 0
  %r = add i64 %e, %x
  ret i64 %r
}
