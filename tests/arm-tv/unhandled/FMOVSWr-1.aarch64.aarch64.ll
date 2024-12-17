define i16 @test1(half %0) {
  %2 = bitcast half %0 to i16
  ret i16 %2
}
