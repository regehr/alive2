; Explicit integer-to-FP rounding modes, including ties, unsigned inputs and half overflow.
define void @fcvt_int_rounding(ptr noundef %out) {
  ; W to S, RUP: 16777217 rounds up to 16777218.
  %p0 = getelementptr i64, ptr %out, i64 0
  store i64 1266679809, ptr %p0, align 1
  ; W to S, RDN: -16777217 rounds down to -16777218.
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 -880803839, ptr %p1, align 1
  ; WU to S, RTZ: UINT32_MAX rounds to 4294967040.
  %p2 = getelementptr i64, ptr %out, i64 2
  store i64 1333788671, ptr %p2, align 1
  ; L to D, RMM: 2^53+1 is a tie rounded away from zero.
  %p3 = getelementptr i64, ptr %out, i64 3
  store i64 4845873199050653697, ptr %p3, align 1
  ; LU to D, RTZ: UINT64_MAX rounds toward zero.
  %p4 = getelementptr i64, ptr %out, i64 4
  store i64 4895412794951729151, ptr %p4, align 1
  ; W to H, RTZ: overflow clamps to the largest finite half.
  %p5 = getelementptr i64, ptr %out, i64 5
  store i64 31743, ptr %p5, align 1
  ; L to S, RMM: negative tie rounds away from zero.
  %p6 = getelementptr i64, ptr %out, i64 6
  store i64 -880803839, ptr %p6, align 1
  ; LU to S, RUP: UINT64_MAX rounds up to 2^64.
  %p7 = getelementptr i64, ptr %out, i64 7
  store i64 1602224128, ptr %p7, align 1
  ret void
}
