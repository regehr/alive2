; CHECK: 1 incorrect transformations

; Different register initializations must not be merged.
define i64 @test() memory(none) {
  ret i64 0
}
