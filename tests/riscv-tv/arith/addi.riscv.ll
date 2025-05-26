; Function Attrs: mustprogress nofree norecurse nosync nounwind willreturn memory(none)
define dso_local noundef i64 @f6(i64 noundef %0, i64 noundef %1) local_unnamed_addr #0 {
  %3 = icmp slt i64 %0, %1
  %4 = add i64 %0, 333
  %5 = sub i64 0, %0
  %6 = select i1 %3, i64 %4, i64 %5
  %7 = add i64 %6, %1
  ret i64 %7
}
