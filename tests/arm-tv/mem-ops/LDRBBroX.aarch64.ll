; ModuleID = '<stdin>'
source_filename = "<stdin>"

define dso_local zeroext i32 @ld_align64_uint32_t_uint8_t(ptr nocapture readonly %0) {
  %2 = getelementptr inbounds i8, ptr %0, i64 1000000000000
  %3 = load i8, ptr %2, align 1
  %4 = zext i8 %3 to i32
  ret i32 %4
}