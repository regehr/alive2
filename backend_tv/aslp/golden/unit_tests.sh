#!/bin/bash

# to be run with:
# goldentests bash backend_tv/aslp/golden '# ' --overwrite
#
# https://github.com/jfecher/golden-tests

tests/lit/lit.py tests/arm-tv/ -s -j12 | grep '\(^FAIL\|Total\)\|%)'

# expected stdout:
# FAIL: Alive2 :: arm-tv/aslp/fptoui.aarch64.ll (classic) (28 of 15078)
# FAIL: Alive2 :: arm-tv/calls/callsite-attr.aarch64.ll (classic) (222 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t1.aarch64.ll (aslp) (517 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t1.aarch64.ll (classic) (518 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t1b.aarch64.ll (aslp) (519 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t1b.aarch64.ll (classic) (520 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t3.aarch64.ll (aslp) (523 of 15078)
# FAIL: Alive2 :: arm-tv/calls/t4.aarch64.ll (aslp) (525 of 15078)
# FAIL: Alive2 :: arm-tv/calls/vec_stack_align_2.aarch64.ll (aslp) (537 of 15078)
# FAIL: Alive2 :: arm-tv/fp/fminmax/fmaxsrr.aarch64.ll (classic) (756 of 15078)
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (aslp) (923 of 15078)
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (classic) (924 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (aslp) (13109 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (classic) (13110 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (aslp) (13115 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (classic) (13116 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (aslp) (13171 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (classic) (13172 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (aslp) (13187 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (classic) (13188 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (aslp) (13205 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (classic) (13206 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (aslp) (13255 of 15078)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (classic) (13256 of 15078)
# FAIL: Alive2 :: arm-tv/stack/STRXui_1.aarch64.ll (classic) (13362 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld1/LD1Fourv2d.aarch64.ll (aslp) (13637 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev4s.aarch64.ll (classic) (13710 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev8h.aarch64.ll (classic) (13716 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv16b_POST.aarch64.ll (classic) (13722 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv4s_POST.aarch64.ll (classic) (13728 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st1/ST1Fourv2d.aarch64.ll (aslp) (13761 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st3/ST3Threev4s.aarch64.ll (classic) (13786 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st4/ST4Fourv4s.aarch64.ll (classic) (13792 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/stp/STPDpre.aarch64.ll (classic) (13798 of 15078)
# FAIL: Alive2 :: arm-tv/vectors/tbl/TBLv16i8Two.aarch64.ll (aslp) (14807 of 15078)
# Total Discovered Tests: 15078
#   Passed   : 14563 (96.58%)
#   Timed Out:   480 (3.18%)
#   Failed   :    35 (0.23%)

