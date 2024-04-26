; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @f() {
  ret i32 42
}

!llvm.module.flags = !{ !1, !2, !3}

!1 = !{i32 8, !"sign-return-address", i32 1}
!2 = !{i32 8, !"sign-return-address-all", i32 1}
!3 = !{i32 8, !"sign-return-address-with-bkey", i32 1}