; ModuleID = '<stdin>'
source_filename = "<stdin>"

define void @storevcsc7(<16 x i8> %0, ptr nocapture %1) {
  %3 = extractelement <16 x i8> %0, i32 7
  store i8 %3, ptr %1, align 1
  ret void
}