; Box the half bits using an integer mask and a double-precision transfer.
define half @nanbox_return_half(i16 noundef zeroext %bits) {
  %value = bitcast i16 %bits to half
  ret half %value
}
