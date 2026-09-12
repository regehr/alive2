; Repeated reads of the same register must agree.
define i64 @test() memory(none) {
  ret i64 0
}
