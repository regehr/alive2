; CHECK: 1 incorrect transformations

; zeroext i33 constrains bits 63:33.
define zeroext i33 @test() {
  ret i33 1
}
