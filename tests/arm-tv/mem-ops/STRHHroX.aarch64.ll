; ModuleID = '<stdin>'
source_filename = "<stdin>"

define dso_local void @st_align32_uint64_t_int16_t(ptr nocapture %0, i64 %1) {
  %3 = trunc i64 %1 to i16
  %4 = getelementptr inbounds i8, ptr %0, i64 99999000
  store i16 %3, ptr %4, align 2
  ret void
}