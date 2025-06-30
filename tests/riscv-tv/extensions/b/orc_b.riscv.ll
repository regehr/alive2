define i32 @tf_0_foo(i1 %cmp144) {
entry:
  %conv149 = zext i1 %cmp144 to i32
  %narrow = mul i32 %conv149, 255
  ret i32 %narrow
}
