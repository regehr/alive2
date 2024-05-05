define <4 x i16> @f(<8 x i16> %0, <4 x i16> %1) {
  %3 = shufflevector <8 x i16> %0, <8 x i16> zeroinitializer, <4 x i32> <i32 5, i32 5, i32 5, i32 5>
  %4 = zext <4 x i16> %3 to <4 x i32>
  %5 = zext <4 x i16> %1 to <4 x i32>
  %new0 = mul <4 x i32> %4, %5
  %last = trunc <4 x i32> %new0 to <4 x i16>
  ret <4 x i16> %last
}