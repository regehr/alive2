; Hardware floating-point argument registers contain NaN-boxed narrow values.
define void @nanbox_args(float noundef %a, half noundef %b, ptr noundef %out) {
  %abits = bitcast float %a to i32
  %az = zext i32 %abits to i64
  %abox = or i64 %az, -4294967296
  store i64 %abox, ptr %out, align 1
  %bbits = bitcast half %b to i16
  %bz = zext i16 %bbits to i64
  %bbox = or i64 %bz, -65536
  %p1 = getelementptr i64, ptr %out, i64 1
  store i64 %bbox, ptr %p1, align 1
  ret void
}
