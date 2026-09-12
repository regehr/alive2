; CHECK: 1 incorrect transformations

; The fixed tp register must retain its entry value.
define void @test() {
  ret void
}
