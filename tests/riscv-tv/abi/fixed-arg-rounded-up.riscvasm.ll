; a vector whose size isn't a power of two is placed as if it were rounded
; up to one: <6 x i32> is treated like <8 x i32>, so b is in v10-v11
define <6 x i32> @test(<6 x i32> %a, <6 x i32> %b) {
  %r = add <6 x i32> %a, %b
  ret <6 x i32> %r
}
