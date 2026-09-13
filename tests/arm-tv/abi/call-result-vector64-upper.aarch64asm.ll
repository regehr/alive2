; CHECK: 1 incorrect transformations

; A call result leaves unused high SIMD register bits unspecified.
declare <2 x i32> @produce()
define i64 @test() {
  %anchor = add i64 0, 0
  %result = call <2 x i32> @produce()
  ret i64 0
}
