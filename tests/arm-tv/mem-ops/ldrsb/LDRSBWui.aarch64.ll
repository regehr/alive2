; ModuleID = '<stdin>'
source_filename = "<stdin>"

@G2 = external hidden global i8

define i16 @test2() {
  %1 = load i8, ptr @G2, align 1
  %2 = sext i8 %1 to i16
  ret i16 %2
}
