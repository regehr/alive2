define <8 x i8> @f(<8 x i8> %0) {
   %2 = insertelement <8 x i8> <i8 74, i8 -71, i8 4, i8 -85, i8 34, i8 -1, i8 -99, i8 -1>, i8 -1, i32 2
   %3 = sdiv <8 x i8> %0, %2
   ret <8 x i8> %3
}
