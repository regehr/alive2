; Repeated reads of the same initial register value must agree.
define i64 @test() memory(none) {
  ret i64 0
}
