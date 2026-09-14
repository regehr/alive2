; CHECK: 1 correct transformations

; Only the poison path may return a value without the required sign extension.
define signext i8 @test(i1 noundef %is_poison) {
  %value = select i1 %is_poison, i8 poison, i8 -1
  ret i8 %value
}
