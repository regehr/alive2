; CHECK: 1 incorrect transformations

; The upper 51 bits of an unattributed i13 argument are unspecified.
define i64 @test(i13 noundef %x) memory(none) {
  ret i64 0
}
