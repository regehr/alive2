; with vl = 0 nothing is written, not even agnostic tail elements
define void @test(ptr %dst, i64 %avl) {
  %z = icmp eq i64 %avl, 0
  call void @llvm.assume(i1 %z)
  store i32 1, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 4
  store i32 1, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %dst, i64 8
  store i32 1, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %dst, i64 12
  store i32 1, ptr %p3, align 4
  ret void
}
declare void @llvm.assume(i1)
