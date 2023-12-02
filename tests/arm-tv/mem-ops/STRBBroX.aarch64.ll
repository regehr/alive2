; ModuleID = '<stdin>'
source_filename = "<stdin>"

define dso_local void @st_align64_uint32_t_uint8_t(ptr nocapture %0, i32 zeroext %1) {
  %3 = trunc i32 %1 to i8
  %4 = getelementptr inbounds i8, ptr %0, i64 1000000000000
  store i8 %3, ptr %4, align 1
  ret void
}