; CHECK: 1 incorrect transformations

; zeroext requires a Boolean to be extended through bit 31.
define zeroext i1 @test() {
  ret i1 true
}
