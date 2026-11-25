; when VLMAX < AVL < 2 * VLMAX, vl is in [ceil(AVL/2), VLMAX]. with
; VLMAX = 4 and AVL in [5, 7], that means vl is 3 or 4
define i64 @test(i64 %avl) {
  %lo = icmp uge i64 %avl, 5
  %hi = icmp ule i64 %avl, 7
  %ok = and i1 %lo, %hi
  call void @llvm.assume(i1 %ok)
  ret i64 1
}
declare void @llvm.assume(i1)
