; ERROR: Value mismatch
; CHECK: 1 incorrect transformations
; FMIN propagates a lone quiet NaN that FMINNM must suppress.
; Arm A64 ISA 2026-06, FPCR.AH=DN=FZ=FZ16=0. Compare result bits.
; Specify ordering with integer keys, independently of floating-point min/max.
define i32 @fminnm_wrong_nan(float noundef %a, float noundef %b) {
  %abits = bitcast float %a to i32
  %bbits = bitcast float %b to i32
  %amag = and i32 %abits, 2147483647
  %bmag = and i32 %bbits, 2147483647
  %an = icmp ugt i32 %amag, 2139095040
  %bn = icmp ugt i32 %bmag, 2139095040
  %aq = and i32 %abits, 4194304
  %bq = and i32 %bbits, 4194304
  %aqzero = icmp eq i32 %aq, 0
  %bqzero = icmp eq i32 %bq, 0
  %as = and i1 %an, %aqzero
  %bs = and i1 %bn, %bqzero
  %aneg = icmp slt i32 %abits, 0
  %bneg = icmp slt i32 %bbits, 0
  %ainv = xor i32 %abits, -1
  %binv = xor i32 %bbits, -1
  %apos = xor i32 %abits, 2147483648
  %bpos = xor i32 %bbits, 2147483648
  %akey = select i1 %aneg, i32 %ainv, i32 %apos
  %bkey = select i1 %bneg, i32 %binv, i32 %bpos
  %choose_a = icmp ult i32 %akey, %bkey
  %ordered = select i1 %choose_a, i32 %abits, i32 %bbits
  %bn_checked = select i1 %bn, i32 %abits, i32 %ordered
  %numeric = select i1 %an, i32 %bbits, i32 %bn_checked
  %first_nan = select i1 %an, i32 %abits, i32 %bbits
  %bs_checked = select i1 %bs, i32 %bbits, i32 %first_nan
  %nan_bits = select i1 %as, i32 %abits, i32 %bs_checked
  %quieted = or i32 %nan_bits, 4194304
  %snan = or i1 %as, %bs
  %both_nan = and i1 %an, %bn
  %propagate = or i1 %snan, %both_nan
  %result = select i1 %propagate, i32 %quieted, i32 %numeric
  ret i32 %result
}
