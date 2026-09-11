; ERROR: Value mismatch
; CHECK: 1 incorrect transformations
; Reject FMAX in place of FMIN: inputs 1.0 and 2.0 must return 1.0.
; Use an independent bitwise specification to check signed zeros and NaNs.
define i64 @fmin_s_wrong_order(float noundef %a, float noundef %b) {
  %abits = bitcast float %a to i32
  %bbits = bitcast float %b to i32
  %a_nan = fcmp uno float %a, %a
  %b_nan = fcmp uno float %b, %b
  %choose_a = fcmp olt float %a, %b
  %chosen = select i1 %choose_a, i32 %abits, i32 %bbits
  %equal = fcmp oeq float %a, %b
  %tie = or i32 %abits, %bbits
  %ordered = select i1 %equal, i32 %tie, i32 %chosen
  %b_checked = select i1 %b_nan, i32 %abits, i32 %ordered
  %a_checked = select i1 %a_nan, i32 %bbits, i32 %b_checked
  %both_nan = and i1 %a_nan, %b_nan
  %bits = select i1 %both_nan, i32 2143289344, i32 %a_checked
  %result = sext i32 %bits to i64
  ret i64 %result
}
