; Arm A64 ISA 2026-06, FPCR.AH=DN=FZ=FZ16=0. Compare result bits.
; Specify ordering with integer keys, independently of floating-point min/max.
define i64 @fmin_d(double noundef %a, double noundef %b) {
  %abits = bitcast double %a to i64
  %bbits = bitcast double %b to i64
  %amag = and i64 %abits, 9223372036854775807
  %bmag = and i64 %bbits, 9223372036854775807
  %an = icmp ugt i64 %amag, 9218868437227405312
  %bn = icmp ugt i64 %bmag, 9218868437227405312
  %aq = and i64 %abits, 2251799813685248
  %bq = and i64 %bbits, 2251799813685248
  %aqzero = icmp eq i64 %aq, 0
  %bqzero = icmp eq i64 %bq, 0
  %as = and i1 %an, %aqzero
  %bs = and i1 %bn, %bqzero
  %aneg = icmp slt i64 %abits, 0
  %bneg = icmp slt i64 %bbits, 0
  %ainv = xor i64 %abits, -1
  %binv = xor i64 %bbits, -1
  %apos = xor i64 %abits, 9223372036854775808
  %bpos = xor i64 %bbits, 9223372036854775808
  %akey = select i1 %aneg, i64 %ainv, i64 %apos
  %bkey = select i1 %bneg, i64 %binv, i64 %bpos
  %choose_a = icmp ult i64 %akey, %bkey
  %ordered = select i1 %choose_a, i64 %abits, i64 %bbits
  %bn_checked = select i1 %bn, i64 %abits, i64 %ordered
  %numeric = select i1 %an, i64 %bbits, i64 %bn_checked
  %first_nan = select i1 %an, i64 %abits, i64 %bbits
  %bs_checked = select i1 %bs, i64 %bbits, i64 %first_nan
  %nan_bits = select i1 %as, i64 %abits, i64 %bs_checked
  %quieted = or i64 %nan_bits, 2251799813685248
  %propagate = or i1 %an, %bn
  %result = select i1 %propagate, i64 %quieted, i64 %numeric
  ret i64 %result
}
