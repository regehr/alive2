define i32 @f(i32 %0) {
  %2 = add nsw i32 %0, 1
  ret i32 %2
}

!llvm.module.flags = !{!0, !1, !2}

!0 = !{i32 8, !"branch-target-enforcement", i32 1}
!1 = !{i32 8, !"sign-return-address", i32 1}
!2 = !{i32 8, !"sign-return-address-all", i32 0}