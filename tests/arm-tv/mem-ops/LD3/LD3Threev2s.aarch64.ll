define <2 x i32> @f(ptr %0) {
  %2 = load <6 x i32>, ptr %0, align 1
  %3 = shufflevector <6 x i32> %2, <6 x i32> undef, <2 x i32> <i32 2, i32 5>
  ret <2 x i32> %3
}