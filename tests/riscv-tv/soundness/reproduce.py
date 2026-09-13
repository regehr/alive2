#!/usr/bin/env python3
"""Check fixed and outstanding soundness cases, optionally with QEMU."""

import argparse
from pathlib import Path
import re
import subprocess
import tempfile


HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[2]
G = 0x20001000
# Concrete results with g linked at G and the pointer argument set to null.
# None denotes a load access fault (mcause=5).
RESULTS = {
    "reloc-high-clobber": (42, G),
    "reloc-base": (G, 0),
    "dead-load": (42, None),
    "call-stack-align": (0, 8),
    "global-subtract": (G + 8, G - 8),
    "global-align": (2, 0),
    "global-layout": (G, 0),
    "global-width": (G, G + (1 << 32)),
}
FIXED = {"call-stack-align"}

LINKER_SCRIPT = """ENTRY(_start)
SECTIONS {
  . = 0x80000000;
  .text : { *(.text.start) *(.text) *(.text.*) }
  . = 0x20000000;
  .rodata : { *(.rodata) }
  . = 0x20001000;
  .rodata.g : { *(.rodata.g) }
}
"""


def run(command, *, timeout=30):
    result = subprocess.run(command, capture_output=True, text=True,
                            timeout=timeout)
    output = result.stdout + result.stderr
    if result.returncode:
        raise RuntimeError(f"{command}\n{output[-4000:]}")
    return output


def require_validation(output, expected):
    # Require a completed proof/counterexample, not an unsupported diagnostic,
    # solver timeout, or an unrelated occurrence of a CHECK string in output.
    match = re.search(r"Summary:\s+(\d+) correct transformations\s+"
                      r"(\d+) incorrect transformations\s+"
                      r"(\d+) failed-to-prove transformations\s+"
                      r"(\d+) Alive2 errors", output)
    counts = tuple(map(int, match.groups())) if match else None
    wanted = (1, 0, 0, 0) if expected == "correct" else (0, 1, 0, 0)
    if counts != wanted:
        raise RuntimeError(f"Expected one {expected} transformation, got "
                           f"{counts}\n{output[-4000:]}")


def harness(expected):
    check_return = "j .Lfail"
    check_trap = "j .Lfail"
    if expected is None:
        check_trap = """csrr t0, mcause
  li t1, 5
  bne t0, t1, .Lfail
  j .Lpass"""
    else:
        check_return = f"""li t0, {expected}
  bne a0, t0, .Lfail
  j .Lpass"""
    return f""".section .text.start
.globl _start
_start:
  li sp, 0x81000000
  la t0, .Ltrap
  csrw mtvec, t0
  li a0, 0
  call test
  {check_return}
.balign 4
.Ltrap:
  {check_trap}
.Lpass:
  li a0, 80
  j .Lprint
.Lfail:
  li a0, 70
.Lprint:
  li t0, 0x10000000
  sb a0, 0(t0)
  li a0, 10
  sb a0, 0(t0)
  li t0, 0x100000
  li t1, 0x5555
  sw t1, 0(t0)
.Lstop:
  j .Lstop
.text
.globl check_alignment
check_alignment:
  andi a0, sp, 15
  ret
.section .rodata.g
.globl g
g:
  .zero 32
"""


def emulate(args, directory, program, expected):
    start = directory / "start.s"
    start.write_text(harness(expected))
    script = directory / "layout.ld"
    script.write_text(LINKER_SCRIPT)
    objects = []
    for index, source in enumerate((start, program)):
        obj = directory / f"input{index}.o"
        run([args.clang, "--target=riscv64-unknown-elf", "-march=rv64gc",
             "-mno-relax", "-O2", "-c", str(source), "-o", str(obj)])
        objects.append(str(obj))
    elf = directory / "test.elf"
    run([args.linker, "--no-relax", "-T", str(script), *objects,
         "-o", str(elf)])
    output = run([args.qemu_bin, "-machine", "virt", "-nographic", "-bios",
                  "none", "-kernel", str(elf), "-no-reboot"], timeout=5)
    if output != "P\n":
        raise RuntimeError(f"Unexpected QEMU result: {output!r}")


def show_result(value):
    return "load access fault" if value is None else f"{value:#x}"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("cases", nargs="*", help="Case names; default: all")
    parser.add_argument("--backend-tv", default=str(ROOT / "build/backend-tv"))
    parser.add_argument("--qemu", action="store_true",
                        help="Also execute source and assembly in QEMU virt")
    parser.add_argument("--clang", default="clang")
    parser.add_argument("--linker", default="ld.lld")
    parser.add_argument("--qemu-bin", default="qemu-system-riscv64")
    args = parser.parse_args()
    cases = args.cases or list(RESULTS)
    for name in cases:
        if name not in RESULTS:
            parser.error(f"Unknown case {name!r}; choose from {', '.join(RESULTS)}")

    base = [args.backend_tv, "-backend=riscv64", "-smt-to=10000"]
    with tempfile.TemporaryDirectory(prefix="riscv-soundness-") as temporary:
        directory = Path(temporary)
        for name in cases:
            ir = HERE / f"{name}.riscvasm.ll"
            asm = HERE / f"{name}.riscvasm.s"
            # A normal compilation of the source must still validate.
            require_validation(run([*base, str(ir)]), "correct")
            output = run([*base, "-asm-input", str(asm), str(ir)])
            require_validation(output, "incorrect" if name in FIXED else "correct")
            status = ("counterexample confirmed" if name in FIXED else
                      "known false approval reproduced")
            print(f"{name}: {status}; codegen control passed",
                  flush=True)
            if name == "dead-load":
                output = run([*base, "-optimize-tgt=sroa", "-asm-input",
                              str(asm), str(ir)])
                require_validation(output, "incorrect")
                print("  SROA-only control rejects the faulting load", flush=True)
            if args.qemu:
                source_result, target_result = RESULTS[name]
                emulate(args, directory, ir, source_result)
                emulate(args, directory, asm, target_result)
                print(f"  QEMU: source {show_result(source_result)}; "
                      f"assembly {show_result(target_result)}", flush=True)


if __name__ == "__main__":
    main()
