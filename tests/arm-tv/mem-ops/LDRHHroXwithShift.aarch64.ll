; ModuleID = '<stdin>'
source_filename = "<stdin>"

; Function Attrs: nounwind
define i16 @extractelt_v16i16_idx(ptr %0, i32 zeroext %1) {
  %3 = load <16 x i16>, ptr %0, align 32
  %4 = extractelement <16 x i16> %3, i32 %1
  ret i16 %4
}