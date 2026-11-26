; a 1024-bit vector takes an m8 group, the largest there is
define <16 x i64> @test(<16 x i64> %a, <16 x i64> %b) {
  %r = xor <16 x i64> %a, %b
  ret <16 x i64> %r
}
