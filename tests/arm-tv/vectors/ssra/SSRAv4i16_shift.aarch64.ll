; Function Attrs: nounwind ssp
define void @f(ptr %0, ptr %1) {
  %3 = load <16 x i8>, ptr %0, align 16
  %4 = shufflevector <16 x i8> %3, <16 x i8> undef, <4 x i32> <i32 0, i32 2, i32 4, i32 6>
  %5 = shufflevector <16 x i8> %3, <16 x i8> undef, <4 x i32> <i32 1, i32 3, i32 5, i32 7>
  %6 = sext <4 x i8> %5 to <4 x i16>
  %7 = sext <4 x i8> %4 to <4 x i16>
  %8 = add <4 x i16> %6, %7
  store <4 x i16> %8, ptr %1, align 8
  ret void
}