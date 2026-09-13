; ERROR: Unsupported Function Return: a 576-bit value would be returned at INDIRECT X8, which we don't support yet

; A result too wide for x0-x7 comes back through a caller-allocated
; buffer whose address arrives in x8, which we do not lift yet.
define i576 @f(ptr %p) {
  %v = load i576, ptr %p
  ret i576 %v
}
