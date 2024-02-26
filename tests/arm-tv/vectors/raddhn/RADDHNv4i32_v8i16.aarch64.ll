; Function Attrs: norecurse nounwind
define void @f(ptr %0) {
  %2 = load i16, ptr %0, align 2
  %3 = insertelement <8 x i16> undef, i16 %2, i32 6
  %4 = insertelement <8 x i16> %3, i16 undef, i32 7
  %5 = sext <8 x i16> %4 to <8 x i32>
  %6 = mul <8 x i32> %5, <i32 -36410, i32 -36410, i32 -36410, i32 -36410, i32 -36410, i32 -36410, i32 -36410, i32 -36410>
  %7 = add <8 x i32> %6, <i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768, i32 32768>
  %8 = add <8 x i32> %7, zeroinitializer
  %9 = ashr <8 x i32> %8, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %10 = add nsw <8 x i32> zeroinitializer, %9
  %11 = shl <8 x i32> %10, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %12 = ashr exact <8 x i32> %11, <i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16, i32 16>
  %13 = sub nsw <8 x i32> zeroinitializer, %12
  %14 = trunc <8 x i32> %13 to <8 x i16>
  %15 = extractelement <8 x i16> %14, i32 7
  store i16 %15, ptr %0, align 2
  ret void
}