; ModuleID = '<stdin>'
source_filename = "<stdin>"

define i8 @test25(ptr %0) {
  %2 = getelementptr inbounds i8, ptr %0, i32 -128
  %3 = load i8, ptr %2, align 1
  ret i8 %3
}