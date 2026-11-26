; vmv.x.s at SEW = 64 needs no sign extension
define i64 @test(ptr %p) {
  %x = load i64, ptr %p, align 8
  ret i64 %x
}
