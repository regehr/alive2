define dso_local i16 @f(ptr %0) local_unnamed_addr {
  %2 = getelementptr inbounds i16, ptr %0, i32 undef
  %3 = load <64 x i16>, ptr undef, align 2
  %4 = shufflevector <64 x i16> %3, <64 x i16> undef, <8 x i32> <i32 3, i32 11, i32 19, i32 27, i32 35, i32 43, i32 51, i32 59>
  %5 = sext <8 x i16> %4 to <8 x i32>
  %6 = mul nsw <8 x i32> %5, <i32 54492, i32 54492, i32 54492, i32 54492, i32 54492, i32 54492, i32 54492, i32 54492>
  %7 = add nsw <8 x i32> %6, <i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768>
  %8 = add <8 x i32> %7, zeroinitializer
  %9 = ashr <8 x i32> %8, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %10 = add nsw <8 x i32> zeroinitializer, %9
  %11 = shl <8 x i32> %10, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %12 = ashr exact <8 x i32> %11, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %13 = add nsw <8 x i32> %12, zeroinitializer
  %14 = trunc <8 x i32> %13 to <8 x i16>
  %15 = extractelement <8 x i16> %14, i32 0
  store i16 %15, ptr %2, align 2
  ret i16 %15
}