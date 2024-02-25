define <4 x i16> @shuffle3(ptr %0) {
  %2 = load <4 x i16>, ptr %0, align 8
  %3 = shufflevector <4 x i16> %2, <4 x i16> undef, <4 x i32> zeroinitializer
  ret <4 x i16> %3
}