; when AVL >= 2 * VLMAX, vl = VLMAX
define i64 @test(i64 %avl) {
  %ok = icmp uge i64 %avl, 8
  call void @llvm.assume(i1 %ok)
  ret i64 4
}
declare void @llvm.assume(i1)
