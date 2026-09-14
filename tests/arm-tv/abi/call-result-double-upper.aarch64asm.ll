; CHECK: 1 incorrect transformations

; A call result leaves unused high SIMD register bits unspecified.
declare double @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = call double @produce()
  ret i64 0
}
