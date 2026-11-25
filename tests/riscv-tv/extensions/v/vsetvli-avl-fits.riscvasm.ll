; when AVL <= VLMAX, vl = AVL
define i64 @test(i64 %avl) {
  %ok = icmp ule i64 %avl, 4
  call void @llvm.assume(i1 %ok)
  ret i64 %avl
}
declare void @llvm.assume(i1)
