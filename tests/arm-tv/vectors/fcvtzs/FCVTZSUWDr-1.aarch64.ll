define i32 @f2(double %0) {
  %2 = fptosi double %0 to i32
  ret i32 %2
}
