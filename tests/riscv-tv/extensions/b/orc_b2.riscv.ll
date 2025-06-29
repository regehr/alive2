define i32 @orc_b_i32(i32 %x) {
entry:
  ; Step 1: OR each byte’s contents down toward bit 0
  %t1 = lshr i32 %x, 8
  %a = or i32 %x, %t1
  %t2 = lshr i32 %a, 16
  %b = or i32 %a, %t2

  ; Step 2: Keep only the low bit of each original byte
  %mask = and i32 %b, 16843009

  ; Step 3: Broadcast that bit to all 8 bits of each byte
  %res = mul i32 %mask, 255

  ret i32 %res
}
