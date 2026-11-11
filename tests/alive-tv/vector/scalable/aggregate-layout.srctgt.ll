; TEST-ARGS: --single-vscale=4 -disable-undef-input

; Each vector has eight bytes of data and a sixteen-byte allocation size.
target datalayout = "e-p:64:64-v16:32"
%pair = type { <vscale x 2 x i8>, <vscale x 2 x i8> }

define <vscale x 2 x i8> @src_load(ptr %p) {
  %a = load %pair, ptr %p, align 4
  %r = extractvalue %pair %a, 1
  ret <vscale x 2 x i8> %r
}

define <vscale x 2 x i8> @tgt_load(ptr %p) {
  %q = getelementptr i8, ptr %p, i64 16
  %r = load <vscale x 2 x i8>, ptr %q, align 4
  ret <vscale x 2 x i8> %r
}

define void @src_store(ptr %p, <vscale x 2 x i8> %a, <vscale x 2 x i8> %b) {
  %x = insertvalue %pair poison, <vscale x 2 x i8> %a, 0
  %y = insertvalue %pair %x, <vscale x 2 x i8> %b, 1
  store %pair %y, ptr %p, align 4
  ret void
}

define void @tgt_store(ptr %p, <vscale x 2 x i8> %a, <vscale x 2 x i8> %b) {
  store <vscale x 2 x i8> %a, ptr %p, align 4
  %q = getelementptr i8, ptr %p, i64 16
  store <vscale x 2 x i8> %b, ptr %q, align 4
  ret void
}

define <vscale x 2 x i8> @src_array(ptr %p) {
  %a = load [2 x <vscale x 2 x i8>], ptr %p, align 4
  %r = extractvalue [2 x <vscale x 2 x i8>] %a, 1
  ret <vscale x 2 x i8> %r
}

define <vscale x 2 x i8> @tgt_array(ptr %p) {
  %q = getelementptr i8, ptr %p, i64 16
  %r = load <vscale x 2 x i8>, ptr %q, align 4
  ret <vscale x 2 x i8> %r
}
