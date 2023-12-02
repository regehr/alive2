; ModuleID = '<stdin>'
source_filename = "<stdin>"

define i32 @test_load_extract_from_mul_4(ptr %0, i32 %1) {
  %3 = mul i32 %1, 510136
  %4 = getelementptr inbounds i8, ptr %0, i32 %3
  %5 = load i8, ptr %4, align 1
  %6 = zext i8 %5 to i32
  ret i32 %6
}