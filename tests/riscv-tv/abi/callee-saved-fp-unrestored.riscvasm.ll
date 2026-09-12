; CHECK: 1 incorrect transformations

; The low 64 bits of fs0 must be preserved, including arbitrary NaN bits.
define void @test() {
  ret void
}
