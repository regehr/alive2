define i32 @f(i32 %0) {
  ret i32 %0
}

!llvm.module.flags = !{!0, !1}

!0 = !{i32 8, !"sign-return-address", i32 1}
!1 = !{i32 8, !"sign-return-address-all", i32 1}