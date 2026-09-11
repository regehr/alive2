; Quiet comparisons return false for either NaN; exception flags are not modeled.
define i64 @fleq_d(double noundef %a, double noundef %b) {
  %cmp = fcmp ole double %a, %b
  %result = zext i1 %cmp to i64
  ret i64 %result
}
