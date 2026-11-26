; a vector smaller than 128 bits is in the low bits of one register
define <2 x i8> @test(<2 x i8> %a, <2 x i8> %b) {
  %r = add <2 x i8> %a, %b
  ret <2 x i8> %r
}
