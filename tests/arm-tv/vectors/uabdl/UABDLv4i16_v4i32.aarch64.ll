; Function Attrs: nocallback nofree nosync nounwind speculatable willreturn memory(none)
declare i32 @llvm.vector.reduce.add.v4i32(<4 x i32>) #0

define i32 @f(<4 x i16> %0, <4 x i16> %1) {
  %3 = zext <4 x i16> %0 to <4 x i32>
  %4 = zext <4 x i16> %1 to <4 x i32>
  %5 = sub nsw <4 x i32> %3, %4
  %6 = icmp slt <4 x i32> %5, zeroinitializer
  %7 = sub nsw <4 x i32> zeroinitializer, %5
  %8 = select <4 x i1> %6, <4 x i32> %7, <4 x i32> %5
  %9 = call i32 @llvm.vector.reduce.add.v4i32(<4 x i32> %8)
  ret i32 %9
}