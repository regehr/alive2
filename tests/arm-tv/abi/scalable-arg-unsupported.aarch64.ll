; CHECK: ERROR: Unsupported function argument
; SVE arguments are not modeled; this used to crash
define i64 @test(<vscale x 2 x i64> %v) {
  ret i64 0
}
