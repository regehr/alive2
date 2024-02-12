define void @extract_store_i16_2(<8 x i16> %0, ptr %1) {
  %3 = extractelement <8 x i16> %0, i32 2
  store i16 %3, ptr %1, align 2
  ret void
}