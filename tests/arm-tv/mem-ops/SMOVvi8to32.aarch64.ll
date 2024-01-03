define i16 @test_v2i8(i16 %0) {
  %2 = bitcast i16 %0 to <2 x i8>
  %3 = extractelement <2 x i8> %2, i64 0
  %4 = extractelement <2 x i8> %2, i64 1
  %5 = sext i8 %3 to i16
  %6 = sext i8 %4 to i16
  %7 = add i16 %5, %6
  ret i16 %7
}