; whole-register loads and stores do not depend on vtype, so they lift
; with no vset{i}vli before them
define void @test(ptr %dst, ptr %src) {
  %lo = load i64, ptr %src, align 1
  %p1 = getelementptr i8, ptr %src, i64 8
  %hi = load i64, ptr %p1, align 1
  store i64 %lo, ptr %dst, align 1
  %q1 = getelementptr i8, ptr %dst, i64 8
  store i64 %hi, ptr %q1, align 1
  ret void
}
