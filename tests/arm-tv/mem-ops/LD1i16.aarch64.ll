; ModuleID = '<stdin>'
source_filename = "<stdin>"

define <8 x i16> @load_lane_i16_a2(ptr %0, <8 x i16> %1) {
  %3 = load i16, ptr %0, align 2
  %4 = insertelement <8 x i16> %1, i16 %3, i32 0
  ret <8 x i16> %4
}