; CHECK: 1 incorrect transformations

; Restoration is required on every reachable return path.
define void @test(i64 %x) {
  ret void
}
