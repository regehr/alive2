; CHECK: 1 incorrect transformations
; a fixed-length vector result has to be returned in v8, not v9
define <4 x i32> @test(<4 x i32> %a, <4 x i32> %b) {
  %r = add <4 x i32> %a, %b
  ret <4 x i32> %r
}
