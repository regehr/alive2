; a fixed-length reduction, which llc lowers to vredsum.vs
define i32 @test(ptr %a) {
  %x = load <8 x i32>, ptr %a, align 4
  %s = call i32 @llvm.vector.reduce.add.v8i32(<8 x i32> %x)
  ret i32 %s
}
