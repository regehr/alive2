; ModuleID = '<stdin>'
source_filename = "<stdin>"

define zeroext i16 @load_R_h_(ptr nocapture readonly %0, i32 %1) local_unnamed_addr {
  %3 = sext i32 %1 to i64
  %4 = getelementptr inbounds i16, ptr %0, i64 %3
  %5 = load i16, ptr %4, align 2
  ret i16 %5
}