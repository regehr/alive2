; ModuleID = '<stdin>'
source_filename = "<stdin>"

define zeroext i16 @store_R_h_(ptr %0, i32 %1, i16 %2) local_unnamed_addr {
  %4 = sext i32 %1 to i64
  %5 = getelementptr inbounds i16, ptr %0, i64 %4
  store i16 %2, ptr %5, align 2
  ret i16 0
}