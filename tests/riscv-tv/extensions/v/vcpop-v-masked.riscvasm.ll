; masked vcpop.v with mask undisturbed: lanes 1 and 3 keep the old
; destination value
define void @test(ptr %d, ptr %a, ptr %b) {
  %x = load <4 x i32>, ptr %a
  %y = load <4 x i32>, ptr %b
  %p = call <4 x i32> @llvm.ctpop.v4i32(<4 x i32> %x)
  %r = select <4 x i1> <i1 true, i1 false, i1 true, i1 false>, <4 x i32> %p, <4 x i32> %y
  store <4 x i32> %r, ptr %d
  ret void
}
