; CHECK: ERROR: Unsupported function argument
; LLVM promotes the elements of <4 x i7> to i8, which we don't model
define <4 x i7> @test(<4 x i7> %a) {
  ret <4 x i7> %a
}
