; CHECK: ERROR: segment access that would use registers past v31
; four fields starting at v30 would need v30-v33
define void @test(ptr %p) {
  ret void
}
