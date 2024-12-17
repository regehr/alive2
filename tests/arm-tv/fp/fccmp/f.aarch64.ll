define i64 @f(double %0, double %1, double %2, double %3, i64 %4, i64 %5) {
  %7 = fcmp ogt double %0, %1
  %8 = fcmp ogt double %2, %3
  %9 = or i1 %7, %8
  %10 = select i1 %9, i64 %4, i64 %5
  ret i64 %10
}
