; CHECK: 1 incorrect transformations

; The upper 32 bits of a narrow zeroext stack slot are unspecified.
define i64 @test(i64 %a, i64 %b, i64 %c, i64 %d, i64 %e, i64 %f, i64 %g, i64 %h, i8 zeroext %x) {
  %result = zext i8 %x to i64
  ret i64 %result
}
