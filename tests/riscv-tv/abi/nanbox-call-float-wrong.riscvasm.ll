; CHECK: 1 incorrect transformations

declare i64 @consume(i64, float)

define i64 @nanbox_call_float_wrong(i64 noundef %tag, i32 noundef %bits) {
  %value = bitcast i32 %bits to float
  %result = call i64 @consume(i64 %tag, float %value)
  ret i64 %result
}
