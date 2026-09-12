; CHECK: 1 incorrect transformations

; Exercise the 128-bit backing register and its upper half.
define i64 @test() memory(none) {
  ret i64 0
}
