/*
 * Everything that talks to ASLP. This is the only translation unit in the
 * lifter that includes an ASLP header, so it is also the only one that
 * needs ANTLR and aslp-cpp on its include path; see the backend_tv_aslp
 * object library in the top-level CMakeLists.
 */

#include "backend_tv/aslp_adapter.h"
#include "backend_tv/aslp_lift.h"

#include "aslp/aslp_bridge.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

#include <format>
#include <stdexcept>

using namespace std;
using namespace lifter;
using namespace llvm;

llvm::AllocaInst *arm_aslp_adapter::get_reg(aslp::reg_t regtype,
                                            uint64_t num) {
  using reg_t = aslp::reg_t;
  using pstate_t = aslp::pstate_t;

  uint64_t reg = 0;
  if (regtype == reg_t::X) {
    if (num <= 28)
      reg = llvm::AArch64::X0 + num;
    else if (num == 29)
      reg = llvm::AArch64::FP;
    else if (num == 30)
      reg = llvm::AArch64::LR;
    else if (num == 31)
      reg = llvm::AArch64::SP;
    else
      assert(false && "X register out of range");

  } else if (regtype == reg_t::PSTATE) {

    if (num == (int)pstate_t::N)
      reg = llvm::AArch64::N;
    else if (num == (int)pstate_t::Z)
      reg = llvm::AArch64::Z;
    else if (num == (int)pstate_t::C)
      reg = llvm::AArch64::C;
    else if (num == (int)pstate_t::V)
      reg = llvm::AArch64::V;

  } else if (regtype == reg_t::V) {
    reg = llvm::AArch64::Q0 + num;
  }

  assert(reg && "register not mapped");
  return llvm::cast<llvm::AllocaInst>(L.RegFile.at(reg));
}

optional<aslp::opcode_t> arm_aslp_adapter::getArmOpcode(const MCInst &I) {
  SmallVector<MCFixup> Fixups{};
  SmallVector<char> Code{};

  if (I.getOpcode() == L.sentinelNOP())
    return nullopt;

  L.MCE->encodeInstruction(I, Code, Fixups, *L.STI.get());

  // do not hand any instructions with relocation fixups to aslp
  if (Fixups.size() != 0)
    return nullopt;

  aslp::opcode_t ret;
  unsigned i = 0;
  for (const char &x : Code) {
    ret.at(i++) = x;
  }
  return ret;
}

bool lifter::tryLiftWithASLP(arm2llvm &L, MCInst &I, StringRef instStr) {
  auto entrybb = L.LLVMBB;
  arm_aslp_adapter adapter{L};
  aslp::bridge bridge{adapter, *L.MCE.get(), *L.STI.get(), *L.IA.get()};

  auto a64Opcode = adapter.getArmOpcode(I);
  if (!a64Opcode) {
    // arm opcode translation failed, possibly SentinelNOP. continue with
    // classic.
    *L.out << "... arm opnum failed: " << instStr.str() << '\n';
    return false;
  }

  auto aslpResult = bridge.run(I, a64Opcode.value());

  if (auto result = std::get_if<aslp::result_t>(&aslpResult)) {
    // branch lifter's entry BB to entry BB in ASLP result,
    // then set ASLP's exit BB to be the next BB.
    L.LLVMBB = entrybb;
    auto [encoding, stmts] = *result;
    L.createBranch(stmts.first);
    L.LLVMBB = stmts.second;
    stmts.first->begin()->setMetadata(
        "asm.aslp",
        llvm::MDTuple::get(L.Ctx, {llvm::MDString::get(L.Ctx, instStr)}));

    *L.out << "... lifted via aslp: " << encoding << " - " << instStr.str()
           << std::endl;
    L.encodingCounts[encoding]++;
    return true;
  }

  switch (std::get<aslp::err_t>(aslpResult)) {
  case aslp::err_t::missing:
    *L.out << "... aslp missing! "
           << std::format("0x{:08x}", aslp::get_opnum(a64Opcode.value()))
           << "  " << aslp::format_opcode(a64Opcode.value()) << std::endl;
    if (aslp::bridge::config().fail_if_missing) {
      throw std::runtime_error(
          "missing aslp instruction in debug mode is not allowed!");
    }
    break;
  case aslp::err_t::banned:
    *L.out << "... aslp banned\n";
    break; // continue with classic.
  }
  return false;
}
