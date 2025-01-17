#!/bin/bash

# to be run with:
# goldentests bash backend_tv/aslp/golden '# ' --overwrite
#
# https://github.com/jfecher/golden-tests

tests/lit/lit.py tests/arm-tv/ -s -j12 | grep '\(^FAIL\|Total\)\|%)'

# expected stdout:
# FAIL: Alive2 :: arm-tv/aslp/fptoui.aarch64.ll (classic) (26 of 15074)
# FAIL: Alive2 :: arm-tv/calls/callsite-attr.aarch64.ll (classic) (218 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t1.aarch64.ll (aslp) (513 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t1.aarch64.ll (classic) (514 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t1b.aarch64.ll (aslp) (515 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t1b.aarch64.ll (classic) (516 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t3.aarch64.ll (aslp) (519 of 15074)
# FAIL: Alive2 :: arm-tv/calls/t4.aarch64.ll (aslp) (521 of 15074)
# FAIL: Alive2 :: arm-tv/calls/vec_stack_align_2.aarch64.ll (aslp) (533 of 15074)
# FAIL: Alive2 :: arm-tv/fp/fminmax/fmaxsrr.aarch64.ll (classic) (752 of 15074)
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (aslp) (919 of 15074)
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (classic) (920 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (aslp) (13105 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (classic) (13106 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (aslp) (13111 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (classic) (13112 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (aslp) (13167 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (classic) (13168 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (aslp) (13183 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (classic) (13184 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (aslp) (13201 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (classic) (13202 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (aslp) (13251 of 15074)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (classic) (13252 of 15074)
# FAIL: Alive2 :: arm-tv/stack/STRXui_1.aarch64.ll (classic) (13358 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld1/LD1Fourv2d.aarch64.ll (aslp) (13633 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev4s.aarch64.ll (classic) (13706 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev8h.aarch64.ll (classic) (13712 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv16b_POST.aarch64.ll (classic) (13718 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv4s_POST.aarch64.ll (classic) (13724 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st1/ST1Fourv2d.aarch64.ll (aslp) (13757 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st3/ST3Threev4s.aarch64.ll (classic) (13782 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st4/ST4Fourv4s.aarch64.ll (classic) (13788 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/stp/STPDpre.aarch64.ll (classic) (13794 of 15074)
# FAIL: Alive2 :: arm-tv/vectors/tbl/TBLv16i8Two.aarch64.ll (aslp) (14803 of 15074)
# Total Discovered Tests: 15074
#   Passed   : 14555 (96.56%)
#   Timed Out:   484 (3.21%)
#   Failed   :    35 (0.23%)

