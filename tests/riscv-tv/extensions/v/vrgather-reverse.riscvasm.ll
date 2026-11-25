; LLVM's vector reverse: vid, vrsub, vrgather
define void @test(ptr %p) {
  %a0 = load i16, ptr %p, align 2
  %p1 = getelementptr i8, ptr %p, i64 2
  %a1 = load i16, ptr %p1, align 2
  %p2 = getelementptr i8, ptr %p, i64 4
  %a2 = load i16, ptr %p2, align 2
  %p3 = getelementptr i8, ptr %p, i64 6
  %a3 = load i16, ptr %p3, align 2
  store i16 %a3, ptr %p, align 2
  store i16 %a2, ptr %p1, align 2
  store i16 %a1, ptr %p2, align 2
  store i16 %a0, ptr %p3, align 2
  ret void
}
