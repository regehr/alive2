#!/usr/bin/env python3
"""
Differential test for backend_tv's model of the C calling convention.

For a randomly generated function signature this compares, argument by
argument and limb by limb, where our CCAssigner says a value is passed
against where the LLVM backend actually passes it.

The oracle is LLVM's own machine IR.  For every (argument, limb) pair we
emit a probe function with the subject's exact signature -- the return
type matters, since an indirectly returned value consumes a0 on RV64 --
whose only job is to store that one limb to a global.  Running llc with
-stop-after=finalize-isel and tracing that store's operand backwards
lands on either a COPY from a physical argument register or a load from a
fixed stack object, which is precisely what we want to know.  Return
values are read off the RET instruction's implicit operands.

Usage:
  abi-difftest.py --abi-dump build/abi-dump --llc path/to/llc [-n 500]
"""

import argparse
import collections
import random
import re
import subprocess
import sys
import tempfile
from pathlib import Path

# (triple, datalayout, argument GPR base in MIR, extra llc flags)
#
# these must match what tools/backend-tv.cpp configures the lifter with,
# or we are comparing against a different ABI: plain riscv64 has no D
# extension and so uses soft-float lp64, where a double travels in a GPR.
TARGETS = {
    "aarch64": (
        "aarch64-unknown-linux-gnu",
        "e-m:e-i8:8:32-i16:16:32-i64:64-i128:128-n32:64-S128-Fn32",
        0,
        [],
    ),
    "riscv64": (
        "riscv64-unknown-linux-gnu",
        "e-m:e-p:64:64-i64:64-i128:128-n32:64-S128",
        10,
        ["-mattr=+rva23u64", "-target-abi=lp64d"],
    ),
}

INT_WIDTHS = [1, 7, 8, 16, 31, 32, 33, 63, 64, 65, 96, 127, 128, 129,
              160, 192, 255, 256, 320, 448, 512, 513, 576, 1024]


FP_TYPES = ["float", "double"]


def rand_type(rng):
    r = rng.random()
    if r < 0.12:
        return "ptr"
    if r < 0.30:
        return rng.choice(FP_TYPES)
    return "i%d" % rng.choice(INT_WIDTHS)


def type_bits(ty):
    if ty == "ptr":
        return 64
    if ty == "float":
        return 32
    if ty == "double":
        return 64
    return int(ty[1:])


def num_limbs(ty):
    if ty in FP_TYPES:
        return 1
    return (type_bits(ty) + 63) // 64


def zero_of(ty):
    if ty == "ptr":
        return "null"
    if ty in FP_TYPES:
        return "0.0"
    return "0"


def limb_body(ty, name, j):
    """IR that puts limb j of value `name` of type `ty` into %v as an i64."""
    if ty == "ptr":
        return ["  %v = ptrtoint ptr {} to i64".format(name)]
    if ty == "double":
        return ["  %v = bitcast double {} to i64".format(name)]
    if ty == "float":
        return ["  %b = bitcast float {} to i32".format(name),
                "  %v = zext i32 %b to i64"]
    w = type_bits(ty)
    if w < 64:
        return ["  %v = zext {} {} to i64".format(ty, name)]
    if w == 64:
        return ["  %v = add i64 {}, 0".format(name)]
    lines = []
    src = name
    if j:
        lines.append("  %s = lshr {} {}, {}".format(ty, src, j * 64))
        src = "%s"
    lines.append("  %v = trunc {} {} to i64".format(ty, src))
    return lines


def make_module(dl, ret, args):
    """Subject function plus one probe per (argument, limb)."""
    sig = ", ".join("%s %%a%d" % (t, i) for i, t in enumerate(args))
    out = ['target datalayout = "%s"' % dl, "@sink = external global i64", ""]
    out.append("define %s @subject(%s) {" % (ret, sig))
    out.append("  ret %s %s" % (ret, zero_of(ret)) if ret != "void"
               else "  ret void")
    out.append("}")

    probes = []
    for i, ty in enumerate(args):
        for j in range(num_limbs(ty)):
            nm = "p_%d_%d" % (i, j)
            probes.append((nm, i, j))
            out.append("")
            out.append("define %s @%s(%s) {" % (ret, nm, sig))
            out += limb_body(ty, "%%a%d" % i, j)
            out.append("  store i64 %v, ptr @sink")
            out.append("  ret %s %s" % (ret, zero_of(ret)) if ret != "void"
                       else "  ret void")
            out.append("}")
    return "\n".join(out) + "\n", probes


# ---------------------------------------------------------------- MIR parsing

VREG_DEF = re.compile(r"^\s*(%\d+)(?::\S+)?\s*=\s*(.*)$")
STORE_SINK = re.compile(r"^\s*\S+\s+(?:killed\s+)?(%\d+),.*@sink")
RET_IMPLICIT = re.compile(r"implicit \$(\w+)")
FIXED_STACK_DECL = re.compile(
    r"^\s*- \{ id: (\d+), type: \S+, offset: (-?\d+), size: (\d+)")


def split_functions(mir):
    funcs, cur, name = {}, [], None
    for line in mir.splitlines():
        if line.startswith("name:"):
            if name:
                funcs[name] = cur
            name = line.split()[1]
            cur = []
        elif name:
            cur.append(line)
    if name:
        funcs[name] = cur
    return funcs


def norm_reg(r, gpr_base):
    """physical GPR name -> argument register index, or None."""
    m = re.fullmatch(r"[wx](\d+)", r)
    if not m:
        return None
    n = int(m.group(1))
    if gpr_base == 0:  # AArch64: x0-x7 are the argument registers, x8 is sret
        return n if n <= 8 else None
    idx = n - gpr_base  # RISC-V MIR spells a0-a7 as x10-x17
    return idx if 0 <= idx <= 7 else None


def phys_to_loc(r, gpr_base):
    """physical register name -> ("reg", i) or ("vec", i), or None."""
    idx = norm_reg(r, gpr_base)
    if idx is not None:
        return ("reg", idx)
    if gpr_base == 0:
        # AArch64 names the FP/vector argument registers by width
        m = re.fullmatch(r"[bhsdq](\d+)", r)
        if m and int(m.group(1)) <= 7:
            return ("vec", int(m.group(1)))
        return None
    # RISC-V MIR spells fa0-fa7 as f10_f / f10_d / ...
    m = re.fullmatch(r"f(\d+)_[fdhq]", r)
    if m:
        i = int(m.group(1)) - gpr_base
        if 0 <= i <= 7:
            return ("vec", i)
    return None


def parse_probe(lines, gpr_base):
    """Where did the value stored to @sink come from?"""
    fixed = {}
    defs = {}
    store_src = None
    in_fixed = False
    for line in lines:
        if line.startswith("fixedStack:"):
            in_fixed = True
            continue
        if in_fixed:
            m = FIXED_STACK_DECL.match(line)
            if m:
                fixed[m.group(1)] = int(m.group(2))
                continue
            if line and not line.startswith(" "):
                in_fixed = False
        m = VREG_DEF.match(line)
        if m:
            defs[m.group(1)] = m.group(2)
            continue
        m = STORE_SINK.match(line)
        if m:
            store_src = m.group(1)
    if store_src is None:
        return None

    # Walk backwards over every virtual-register operand.  A single limb
    # reaches exactly one argument register or fixed stack object, but it
    # may get there through several instructions, and some of them have
    # operands that go nowhere -- the INSERT_SUBREG that widens a narrow
    # argument takes an IMPLICIT_DEF as its first operand, so following
    # only the first operand would dead-end.
    found, seen, work = set(), set(), [store_src]
    while work:
        cur = work.pop()
        if cur in seen or cur not in defs:
            continue
        seen.add(cur)
        rhs = defs[cur]
        m = re.search(r"COPY \$(\w+)", rhs)
        if m:
            loc = phys_to_loc(m.group(1), gpr_base)
            if loc is not None:
                found.add(loc)
            continue
        m = re.search(r"%fixed-stack\.(\d+)", rhs)
        if m:
            found.add(("stack", fixed.get(m.group(1))))
            continue
        work += re.findall(r"(%\d+)", rhs)
    return found.pop() if len(found) == 1 else None


def parse_ret(lines, gpr_base):
    """Return registers off the RET instruction; empty when indirect."""
    for line in lines:
        if "RET_ReallyLR" in line or "PseudoRET" in line:
            regs = [norm_reg(r, gpr_base) for r in RET_IMPLICIT.findall(line)]
            regs = [r for r in regs if r is not None]
            return regs
    return []


def block_liveins(lines):
    """physical registers live into the entry block.

    a MIR function has a function-level `liveins:` field of its own, which
    is empty here, so only look inside the body.
    """
    in_body = False
    for line in lines:
        if line.startswith("body:"):
            in_body = True
            continue
        st = line.strip()
        if in_body and st.startswith("liveins:"):
            return set(re.findall(r"\$(\w+)", st))
    return set()


# ------------------------------------------------------------------- compare

def decode_loc(tok):
    """one token of abi-dump output -> the tuple parse_probe returns."""
    kinds = {"r": "reg", "v": "vec", "s": "stack"}
    return (kinds[tok[0]], int(tok[1:]))


def parse_abi_dump(text):
    out = {}
    for line in text.splitlines():
        f = line.split()
        if len(f) >= 3:
            out[(f[0], f[1])] = f[2:]
    return out


def check(rng, tgt, abi_dump, llc, tmp, counts):
    triple, dl, gpr_base, extra = TARGETS[tgt]
    ret = "void" if rng.random() < 0.25 else rand_type(rng)
    args = [rand_type(rng) for _ in range(rng.randint(0, 14))]
    mod, probes = make_module(dl, ret, args)

    path = tmp / "m.ll"
    path.write_text(mod)

    dump = subprocess.run([abi_dump, str(path), "-target", tgt],
                          capture_output=True, text=True)
    if dump.returncode:
        return ["abi-dump failed:\n" + dump.stderr]
    ours = parse_abi_dump(dump.stdout)

    mir = subprocess.run([llc, "-mtriple=" + triple] + extra +
                         ["-stop-after=finalize-isel", str(path), "-o", "-"],
                         capture_output=True, text=True)
    if mir.returncode:
        return ["llc failed:\n" + mir.stderr]
    funcs = split_functions(mir.stdout)

    bad = []
    sig = "%s (%s)" % (ret, ", ".join(args))

    # return value
    want = ours.get(("subject", "ret"))
    got = parse_ret(funcs.get("subject", []), gpr_base)
    if ret == "void":
        expect = []
    elif want[0] == "DIRECT":
        expect = [int(r[1:]) for r in want[1:]]
    else:
        expect = []  # indirect: nothing comes back in a return register
    counts["ret"] += 1
    if got != expect:
        bad.append("%s: ret: we say %s, llc uses %s" % (sig, want, got))
    elif ret != "void" and want[0] == "INDIRECT":
        # the RET instruction says nothing about where the caller's buffer
        # address arrived, so check it came in where we said it did
        live = block_liveins(funcs.get("subject", []))
        reg = "x8" if tgt == "aarch64" else "x%d" % gpr_base
        counts["indirect-ret"] += 1
        if reg not in live:
            bad.append("%s: ret: we say the buffer address is in %s, "
                       "but llc's live-ins are %s" % (sig, reg, sorted(live)))

    # arguments
    for nm, i, j in probes:
        counts["limb"] += 1
        want = ours.get(("subject", "arg%d" % i))
        got = parse_probe(funcs.get(nm, []), gpr_base)
        if got is None:
            bad.append("%s: arg%d limb%d: could not read llc's choice" %
                       (sig, i, j))
            continue
        if want[0] == "INDIRECT":
            # only the pointer is placed; limbs live in the buffer
            k = want[1]
            expect = decode_loc(k)
            # every limb probe loads through the same pointer, so all we can
            # confirm here is that the pointer itself is where we said
            counts["indirect"] += 1
            if got != expect:
                bad.append("%s: arg%d (indirect): we say %s, llc uses %s" %
                           (sig, i, want, got))
            continue
        k = want[1 + j]
        expect = decode_loc(k)
        counts["stack" if k[0] == "s" else "reg"] += 1
        if got != expect:
            bad.append("%s: arg%d limb%d: we say %s, llc uses %s" %
                       (sig, i, j, k, got))
    return bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--abi-dump", default="build/abi-dump")
    ap.add_argument("--llc", default="llc")
    ap.add_argument("-n", type=int, default=200)
    ap.add_argument("--seed", type=int, default=0)
    ap.add_argument("--target", choices=list(TARGETS) + ["both"],
                    default="both")
    a = ap.parse_args()

    rng = random.Random(a.seed)
    tgts = list(TARGETS) if a.target == "both" else [a.target]
    fails = 0
    with tempfile.TemporaryDirectory() as td:
        tmp = Path(td)
        for t in tgts:
            counts = collections.Counter()
            for _ in range(a.n):
                for msg in check(rng, t, a.abi_dump, a.llc, tmp, counts):
                    print("%s: %s" % (t, msg))
                    fails += 1
            print("%s: %d returns, %d limbs checked "
                  "(%d in registers, %d on the stack, %d indirect)" %
                  (t, counts["ret"], counts["limb"], counts["reg"],
                   counts["stack"], counts["indirect"]))
            print("%s: %d indirect returns verified" %
                  (t, counts["indirect-ret"]))
    print("%d mismatches" % fails)
    return 1 if fails else 0


if __name__ == "__main__":
    sys.exit(main())
