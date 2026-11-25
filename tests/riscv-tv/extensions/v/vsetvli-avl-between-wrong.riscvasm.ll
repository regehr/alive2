; CHECK: 1 incorrect transformations
; when VLMAX < AVL < 2 * VLMAX, vl may be anything in [ceil(AVL/2), VLMAX],
; so it is not necessarily min(AVL, VLMAX)
define i64 @test(i64 %avl) {
  %r = call i64 @llvm.umin.i64(i64 %avl, i64 4)
  ret i64 %r
}
declare i64 @llvm.umin.i64(i64, i64)
