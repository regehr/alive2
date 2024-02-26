; Function Attrs: nounwind
define <16 x i8> @f(ptr %0, ptr %1) {
  %3 = load <16 x i8>, ptr %0, align 16
  %4 = load <16 x i8>, ptr %1, align 16
  %5 = ashr <16 x i8> %4, <i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7, i8 7>
  %6 = add <16 x i8> %3, %5
  ret <16 x i8> %6
}