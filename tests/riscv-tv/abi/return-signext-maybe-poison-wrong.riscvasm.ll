; CHECK: 1 incorrect transformations

; Only the poison path can use an arbitrary return representation.
define signext i8 @test(i1 noundef zeroext %is_poison) {
  %value = select i1 %is_poison, i8 poison, i8 -1
  ret i8 %value
}
