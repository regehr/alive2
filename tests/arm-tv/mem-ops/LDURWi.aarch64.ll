; ModuleID = '<stdin>'
source_filename = "<stdin>"

define i32 @test15(ptr %0) {
  %2 = getelementptr inbounds i32, ptr %0, i32 -64
  %3 = load i32, ptr %2, align 4
  ret i32 %3
}