; A valid box must preserve every payload, including signaling NaNs.
define float @nanbox_return_float(i32 noundef %bits) {
  %value = bitcast i32 %bits to float
  ret float %value
}
