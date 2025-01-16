#!/bin/bash

# to be run with:
# goldentests bash backend_tv/aslp/golden '# ' --overwrite
#
# https://github.com/jfecher/golden-tests

tests/lit/lit.py tests/arm-tv/ -s -j12 | grep '\(^FAIL\|Total\)\|%)'

# expected stdout:
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (aslp) (913 of 15068)
# FAIL: Alive2 :: arm-tv/globals/global-test-51.aarch64.ll (classic) (914 of 15068)
# FAIL: Alive2 :: arm-tv/mem-ops/strb/STRBBpost.aarch64.ll (aslp) (4329 of 15068)
# FAIL: Alive2 :: arm-tv/mem-ops/strb/STRBBpost.aarch64.ll (classic) (4330 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (aslp) (13099 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-10.aarch64.ll (classic) (13100 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (aslp) (13105 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-13.aarch64.ll (classic) (13106 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (aslp) (13161 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-4.aarch64.ll (classic) (13162 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (aslp) (13177 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-47.aarch64.ll (classic) (13178 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (aslp) (13195 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-55.aarch64.ll (classic) (13196 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (aslp) (13245 of 15068)
# FAIL: Alive2 :: arm-tv/pointers/offset-test-78.aarch64.ll (classic) (13246 of 15068)
# FAIL: Alive2 :: arm-tv/stack/STRXui_1.aarch64.ll (classic) (13352 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev4s.aarch64.ll (classic) (13700 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev8h.aarch64.ll (classic) (13706 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv16b_POST.aarch64.ll (classic) (13712 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/ld4/LD4Fourv4s_POST.aarch64.ll (classic) (13718 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st3/ST3Threev4s.aarch64.ll (classic) (13776 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/mem-ops/st4/ST4Fourv4s.aarch64.ll (classic) (13782 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/neg/NEGv1i64-1.aarch64.ll (aslp) (13909 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/tbl/TBLv16i8Two.aarch64.ll (aslp) (14797 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/ushl/USHLv1i64.aarch64.ll (aslp) (14981 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/ushr/USHRd.aarch64.ll (aslp) (15001 of 15068)
# FAIL: Alive2 :: arm-tv/vectors/usra/USRAd.aarch64.ll (aslp) (15009 of 15068)
# Total Discovered Tests: 15068
#   Passed   : 14559 (96.62%)
#   Timed Out:   481 (3.19%)
#   Failed   :    28 (0.19%)

