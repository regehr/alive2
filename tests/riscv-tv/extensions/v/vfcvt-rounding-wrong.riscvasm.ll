; CHECK: 1 incorrect transformations
; fptosi truncates, but vfcvt.x.f.v rounds with frm (to nearest)
define <2 x i32> @test(<2 x float> %x) {
  %r = fptosi <2 x float> %x to <2 x i32>
  ret <2 x i32> %r
}
