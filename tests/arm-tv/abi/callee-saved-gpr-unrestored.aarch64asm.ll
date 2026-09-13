; CHECK: 1 incorrect transformations

; x19 must retain its incoming value.
define void @test() {
  ret void
}
