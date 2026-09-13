; CHECK: 1 correct transformations

; Both paths maintain alignment and restore SP.
define i64 @test(i64 noundef %x) {
  ret i64 42
}
