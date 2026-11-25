; a two-word add: vmadc.vv produces the carry out of the low words into
; v0, and vadc.vvm adds it into the high words
define void @test(ptr %dst, ptr %alo, ptr %blo, ptr %ahi, ptr %bhi) {
  %al = load i32, ptr %alo, align 4
  %bl = load i32, ptr %blo, align 4
  %ah = load i32, ptr %ahi, align 4
  %bh = load i32, ptr %bhi, align 4
  %lo = add i32 %al, %bl
  %c = icmp ult i32 %lo, %al
  %cz = zext i1 %c to i32
  %h0 = add i32 %ah, %bh
  %hi = add i32 %h0, %cz
  store i32 %lo, ptr %dst, align 4
  %p1 = getelementptr i8, ptr %dst, i64 4
  store i32 %hi, ptr %p1, align 4
  ret void
}
