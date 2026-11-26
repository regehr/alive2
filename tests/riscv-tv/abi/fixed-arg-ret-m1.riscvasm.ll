; LLVM passes fixed-length vectors in vector registers too: a 128-bit
; vector takes one register
define <4 x i32> @test(<4 x i32> %a, <4 x i32> %b) {
  %r = add <4 x i32> %a, %b
  ret <4 x i32> %r
}
