define <16 x i8> @vector_add_16_8(<16 x i8> %a, <16 x i8> %b) {
    %c = add <16 x i8> %a, %b
    ret <16 x i8> %c
}