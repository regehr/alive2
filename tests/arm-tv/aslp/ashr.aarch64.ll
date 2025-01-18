define <8 x i8> @f(<8 x i8> %0, <8 x i8> %1) {
   %3 = ashr <8 x i8> %1, %0
   ret <8 x i8> %3
}
