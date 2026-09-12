; CHECK: 1 incorrect transformations

declare i64 @consume(float, half)

define i64 @nanbox_tail_half_wrong(float noundef %first, i16 noundef zeroext %bits) {
  %value = bitcast i16 %bits to half
  %result = tail call i64 @consume(float %first, half %value)
  ret i64 %result
}
