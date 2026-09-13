; CHECK: 1 incorrect transformations

; Only the nonzero input path temporarily misaligns SP.
define i64 @test(i64 noundef %x) {
  ret i64 42
}
