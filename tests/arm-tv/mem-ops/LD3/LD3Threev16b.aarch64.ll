define <16 x i8> @f(ptr %0) {
  %2 = load <48 x i8>, ptr %0, align 64
  %3 = shufflevector <48 x i8> %2, <48 x i8> poison, <16 x i32> <i32 0, i32 3, i32 6, i32 9, i32 12, i32 15, i32 18, i32 21, i32 24, i32 27, i32 30, i32 33, i32 36, i32 39, i32 42, i32 45>
  %4 = shufflevector <48 x i8> %2, <48 x i8> poison, <16 x i32> <i32 1, i32 4, i32 7, i32 10, i32 13, i32 16, i32 19, i32 22, i32 25, i32 28, i32 31, i32 34, i32 37, i32 40, i32 43, i32 46>
  %5 = shufflevector <48 x i8> %2, <48 x i8> poison, <16 x i32> <i32 2, i32 5, i32 8, i32 11, i32 14, i32 17, i32 20, i32 23, i32 26, i32 29, i32 32, i32 35, i32 38, i32 41, i32 44, i32 47>
  %6 = add <16 x i8> %3, %4
  %7 = add <16 x i8> %5, %6
  ret <16 x i8> %7
}