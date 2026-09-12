; CHECK: 1 incorrect transformations

; Every return path must preserve s0-s11.
define i64 @test(i64 noundef %condition) {
  ret i64 42
}
