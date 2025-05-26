; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local signext i16 @f(i16 noundef signext %0, i16 noundef signext %1) local_unnamed_addr #0 {
  %3 = sext i16 %1 to i32
  %4 = sext i16 %0 to i32
  %5 = sub nsw i32 %4, %3
  %6 = icmp slt i32 %5, -33
  %7 = sext i1 %6 to i16
  %8 = add i16 %7, %1
  ret i16 %8
}
