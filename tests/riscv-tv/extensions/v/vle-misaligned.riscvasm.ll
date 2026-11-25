; CHECK: 1 incorrect transformations
; RVA23U64 does not promise that misaligned vector element accesses work,
; so the lifter treats them as faults; the source is fine with align 1
define void @test(ptr %dst, ptr %src) {
  %v = load i64, ptr %src, align 1
  store i64 %v, ptr %dst, align 1
  ret void
}
