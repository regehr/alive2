; !nontemporal loads and stores; llc emits ntl.all hints for them
define void @test(ptr %d, ptr %s) {
  %v = load i32, ptr %s, align 4, !nontemporal !0
  %w = add i32 %v, 1
  store i32 %w, ptr %d, align 4, !nontemporal !0
  ret void
}

!0 = !{i32 1}
