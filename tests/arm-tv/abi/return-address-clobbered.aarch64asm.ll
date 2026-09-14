; CHECK: 1 incorrect transformations

; Returning must resume at the caller's address.
define void @test() {
  ret void
}
