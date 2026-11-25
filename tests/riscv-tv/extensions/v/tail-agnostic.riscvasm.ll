; with ta, each tail element either keeps its old value or becomes all
; ones, chosen independently; the source allows each of those outcomes
define void @test(ptr %dst) {
  %t2 = freeze i1 poison
  %t3 = freeze i1 poison
  %e2 = select i1 %t2, i32 1, i32 -1
  %e3 = select i1 %t3, i32 1, i32 -1
  store i32 7, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 4
  store i32 7, ptr %p1, align 4
  %p2 = getelementptr i8, ptr %dst, i64 8
  store i32 %e2, ptr %p2, align 4
  %p3 = getelementptr i8, ptr %dst, i64 12
  store i32 %e3, ptr %p3, align 4
  ret void
}
