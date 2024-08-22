
to find tests which failed in only aslp:
```bash
python3 -c '[print(x) for x in open("/dev/stdin").read().split("\n\n") if "\n" not in x]' < tests-after-sdiv.txt | grep aslp
```

to copy these into a new folder, append:
```bash
... | cut -d' ' -f4 | xargs -I'{}' cp -v 'tests/{}' retry-lit-fail
```

this can then be used with run-arm-tv.pl:
```bash
cd retry-lit-fail
../backend_tv/scripts/run-arm-tv.pl .
```
(cd first to avoid clashing with usual logs and logs-aslp folders)

to classify the outputs:
```
../backend_tv/scripts/classify.pl logs-aslp
```


# 2024-08-22

We have just fixed ASLp's sdiv implementation by using an explicit branch
instead of clever select tricks.

classic timeouts:
```
TIMEOUT: Alive2 :: arm-tv/globals/global-test-11.aarch64.ll (classic) (726 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-3.aarch64.ll (classic) (766 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-31.aarch64.ll (classic) (770 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-40.aarch64.ll (classic) (790 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-75.aarch64.ll (classic) (866 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-88.aarch64.ll (classic) (894 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-97.aarch64.ll (classic) (914 of 14968)
TIMEOUT: Alive2 :: arm-tv/mem-ops/load_store_ints/test_120.aarch64.ll (classic) (3878 of 14968)
TIMEOUT: Alive2 :: arm-tv/mixed-widths/i16-1328.aarch64.ll (classic) (4800 of 14968)
TIMEOUT: Alive2 :: arm-tv/pointers/offset-test-20.aarch64.ll (classic) (13022 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/mem-ops/ld3/LD3Threev8b.aarch64.ll (classic) (13604 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/not/test_2_32.aarch64.ll (classic) (13828 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/sli/SLIv16i8_shift.aarch64.ll (classic) (14496 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smull/SMULLv4i16_indexed.aarch64.ll (classic) (14534 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uaddlp/UADDLPv8i8_v4i16.aarch64.ll (classic) (14764 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uhadd/UHADDv2i32.aarch64.ll (classic) (14792 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uhadd/UHADDv4i16.aarch64.ll (classic) (14794 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uhadd/UHADDv4i32.aarch64.ll (classic) (14796 of 14968)
```

aslp timeouts:
```
TIMEOUT: Alive2 :: arm-tv/bugs/function_switch.aarch64.ll (aslp) (91 of 14968)
TIMEOUT: Alive2 :: arm-tv/bugs/function_switch_self.aarch64.ll (aslp) (93 of 14968)
TIMEOUT: Alive2 :: arm-tv/calls/float.aarch64.ll (aslp) (213 of 14968)
TIMEOUT: Alive2 :: arm-tv/calls/stack_args_ptr.aarch64.ll (aslp) (495 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-21.aarch64.ll (aslp) (747 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-24.aarch64.ll (aslp) (753 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-66.aarch64.ll (aslp) (845 of 14968)
TIMEOUT: Alive2 :: arm-tv/globals/global-test-72.aarch64.ll (aslp) (859 of 14968)
TIMEOUT: Alive2 :: arm-tv/pointers/STPXpre-LDPXpost-2.aarch64.ll (aslp) (12985 of 14968)
TIMEOUT: Alive2 :: arm-tv/pointers/offset-test-64.aarch64.ll (aslp) (13115 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/mem-ops/ld1/LD1Fourv2d.aarch64.ll (aslp) (13527 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/mem-ops/st2/ST2Twov4s.aarch64.ll (aslp) (13663 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/mem-ops/st3/ST3Threev4h.aarch64.ll (aslp) (13673 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/sadalp/SADALPv8i16_v4i32.aarch64.ll (aslp) (14413 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/saddlp/SADDLPv8i16_v4i32.aarch64.ll (aslp) (14427 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/shrn/SHRNv2i32_shift.aarch64.ll (aslp) (14485 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/sli/SLIv2i64_shift.aarch64.ll (aslp) (14499 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smlal/SMLALv2i32_v2i64.aarch64.ll (aslp) (14513 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smlal/SMLALv4i16_v4i32.aarch64.ll (aslp) (14515 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smlal/SMLALv8i8_v8i16.aarch64.ll (aslp) (14519 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smlsl/SMLSLv2i32_v2i64.aarch64.ll (aslp) (14521 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/smlsl/SMLSLv4i16_v4i32.aarch64.ll (aslp) (14523 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/sqsub/SQSUBv2i64.aarch64.ll (aslp) (14545 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/srhadd/SRHADDv8i8.aarch64.ll (aslp) (14563 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/sub-mem/test_8_32.aarch64.ll (aslp) (14665 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uabd/UABDv8i16.aarch64.ll (aslp) (14733 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/uadalp/UADALPv8i16_v4i32.aarch64.ll (aslp) (14747 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/umlal/UMLALv8i16_v4i32.aarch64.ll (aslp) (14809 of 14968)
TIMEOUT: Alive2 :: arm-tv/vectors/urhadd/URHADDv8i16.aarch64.ll (aslp) (14875 of 14968)
```

many of these timeouts are transient and only fail when running the test suite in parallel.
this exacerbated by my (Kait) laptop's heterogeneous CPU architecture,
with 4P + 8E cores.
of course, this still indicates a performance deficiency with ASLp as they verify much
faster with classic.
however, it does give us confidence the ASLp semantics are equivalently correct.

the classified outputs after retrying are:
```
logs-aslp/float.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/function_switch.aarch64.ll_0.log|[u] Timeout
logs-aslp/function_switch_self.aarch64.ll_0.log|[u] Timeout
logs-aslp/global-test-21.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/global-test-24.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/global-test-66.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/global-test-72.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/LD1Fourv2d.aarch64.ll_0.log|[u] Timeout
logs-aslp/offset-test-64.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/SADALPv8i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/SADDLPv8i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/SHRNv2i32_shift.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/SLIv2i64_shift.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/SMLALv2i32_v2i64.aarch64.ll_0.log|[u] Timeout
logs-aslp/SMLALv4i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/SMLALv8i8_v8i16.aarch64.ll_0.log|[u] Timeout
logs-aslp/SMLSLv2i32_v2i64.aarch64.ll_0.log|[u] Timeout
logs-aslp/SMLSLv4i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/SQSUBv2i64.aarch64.ll_0.log|[u] Timeout
logs-aslp/SRHADDv8i8.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/ST2Twov4s.aarch64.ll_0.log|[u] Timeout
logs-aslp/ST3Threev4h.aarch64.ll_0.log|[u] Timeout
logs-aslp/stack_args_ptr.aarch64.ll_0.log|[u] Timeout
logs-aslp/STPXpre-LDPXpost-2.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/test_8_32.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/UABDv8i16.aarch64.ll_0.log|[c] Translation validated as correct
logs-aslp/UADALPv8i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/UMLALv8i16_v4i32.aarch64.ll_0.log|[u] Timeout
logs-aslp/URHADDv8i16.aarch64.ll_0.log|[c] Translation validated as correct
```

of the aslp timeouts,

- SLIv2i64_shift.aarch64.ll is a vectoriser deficiency. succeeds in isolation after 30 seconds.
  pseudocode is:
  ```
  for e = 0 to elements-1
    shifted = LSL(Elem[operand, e, esize], shift);
    Elem[result, e, esize] = (Elem[operand2, e, esize] AND NOT(mask)) OR shifted;
  V[d] = result;
  ```
  this causes big problems for vectoriser,
  as the operation essentially needs to be "transposed".
  where the spec performs an elementwise AND, LSL, and OR, the ideal vector representation
  is a vector LSL, vector AND, then vector OR.

- UADALP, a pairwise reduce add, is also tricky
- SMLAL is hindered by using uniform vector operations, e.g. bitwise AND/OR/XOR.
  for these, the specification does not use any loop structures or vector accessors,
  so there is nothing for the vectoriser to notice.
  these always partially evaluate to an i128 operation.

- arm-tv/calls/float.aarch64.ll succeeds in isolation after 11s.
- arm-tv/globals/global-test-21.aarch64.ll after 6s.
- arm-tv/globals/global-test-24.aarch64.ll after 12s.

- ST2Twov4s.aarch64.ll succeeds after 40s. 
  the source is a shufflevec with poison, followed by a store.
  and the target is an elementwise store into offsets of the destination pointer.
  also realises poison behaviour.
- ST3Threev4h.aarch64.ll succeeds after 90s.
