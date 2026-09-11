; Quiet comparisons return false for either NaN; exception flags are not modeled.
define i64 @fleq_s(float noundef %a, float noundef %b) {
  %cmp = fcmp ole float %a, %b
  %result = zext i1 %cmp to i64
  ret i64 %result
}
