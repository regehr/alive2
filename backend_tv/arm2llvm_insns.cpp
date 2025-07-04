#include "backend_tv/arm2llvm.h"

#include "Target/AArch64/MCTargetDesc/AArch64MCAsmInfo.h"

#define AARCH64_MAP_IMPL
#include "aslp/aarch64_map.h"
#include "aslp/aslp_bridge.h"

using namespace std;
using namespace lifter;
using namespace llvm;

void arm2llvm::lift(MCInst &I) {
  auto entrybb = LLVMBB;
  aslp::bridge bridge{*this, *MCE.get(), *STI.get(), *IA.get()};
  auto opcode = I.getOpcode();

  StringRef instStr = InstPrinter->getOpcodeName(I.getOpcode());
  if (auto a64Opcode = getArmOpcode(I)) {
    auto aslpResult = bridge.run(I, a64Opcode.value());

    if (auto result = std::get_if<aslp::result_t>(&aslpResult)) {

      // branch lifter's entry BB to entry BB in ASLP result,
      // then set ASLP's exit BB to be the next BB.
      LLVMBB = entrybb;
      auto [encoding, stmts] = *result;
      this->createBranch(stmts.first);
      LLVMBB = stmts.second;
      stmts.first->begin()->setMetadata(
          "asm.aslp",
          llvm::MDTuple::get(Ctx, {llvm::MDString::get(Ctx, instStr)}));

      *out << "... lifted via aslp: " << encoding << " - " << instStr.str()
           << std::endl;
      encodingCounts[encoding]++;
      return;

    } else {
      switch (std::get<aslp::err_t>(aslpResult)) {
      case aslp::err_t::missing:
        *out << "... aslp missing! "
             << std::format("0x{:08x}", aslp::get_opnum(a64Opcode.value()))
             << "  " << aslp::format_opcode(a64Opcode.value()) << std::endl;
        if (aslp::bridge::config().fail_if_missing) {
          throw std::runtime_error(
              "missing aslp instruction in debug mode is not allowed!");
        }
        break;
      case aslp::err_t::banned:
        *out << "... aslp banned\n";
        break; // continue with classic.
      }
    }
  } else {
    *out << "... arm opnum failed: "
         << InstPrinter->getOpcodeName(I.getOpcode()).str() << '\n';
    // arm opcode translation failed, possibly SentinelNOP. continue with
    // classic.
  }

  std::string encoding{"classic_" + aslp::aarch64_revmap().at(opcode)};
  encodingCounts[encoding]++;

  // always create new bb per instruction, to match aslp
  auto newbb = BasicBlock::Create(Ctx, "lifter_" + nextName(), liftedFn);
  createBranch(newbb);
  LLVMBB->getTerminator()->setMetadata(
      "asm.classic",
      llvm::MDTuple::get(Ctx, {llvm::MDString::get(Ctx, instStr)}));

  LLVMBB = newbb;

  // auto i1 = getIntTy(1);
  // auto i8 = getIntTy(8);
  // auto i16 = getIntTy(16);
  // auto i32 = getIntTy(32);
  // auto i64 = getIntTy(64);

  switch (opcode) {

  // nops
  case AArch64::PRFMl:
  case AArch64::PRFMroW:
  case AArch64::PRFMroX:
  case AArch64::PRFMui:
  case AArch64::PRFUMi:
  case AArch64::PACIASP:
  case AArch64::PACIBSP:
  case AArch64::AUTIASP:
  case AArch64::AUTIBSP:
  case AArch64::HINT:
    break;

  // we're abusing this opcode as the sentinel for basic blocks,
  // shouldn't be a problem
  case AArch64::SEH_Nop:
    assert(AArch64::SEH_Nop == sentinelNOP());
    break;

  case AArch64::BRK:
    // FIXME -- look at the argument and emit ubsan_trap if appropriate
    createTrap();
    createUnreachable();
    break;

  case AArch64::BL:
    doDirectCall();
    break;

  case AArch64::B:
    lift_branch();
    break;

  case AArch64::BR:
    doIndirectCall();
    doReturn();
    break;

  case AArch64::BLR:
    doIndirectCall();
    break;

  case AArch64::RET:
    doReturn();
    break;

  case AArch64::Bcc:
    lift_bcc();
    break;

  case AArch64::MRS:
    lift_mrs();
    break;

  case AArch64::MSR:
    lift_msr();
    break;

  case AArch64::ADDWrs:
  case AArch64::ADDWri:
  case AArch64::ADDWrx:
  case AArch64::ADDSWrs:
  case AArch64::ADDSWri:
  case AArch64::ADDSWrx:
  case AArch64::ADDXrs:
  case AArch64::ADDXri:
  case AArch64::ADDXrx:
  case AArch64::ADDSXrs:
  case AArch64::ADDSXri:
  case AArch64::ADDSXrx:
    lift_add(opcode);
    break;

  case AArch64::ADCWr:
  case AArch64::ADCXr:
  case AArch64::ADCSWr:
  case AArch64::ADCSXr:
  case AArch64::SBCWr:
  case AArch64::SBCXr:
  case AArch64::SBCSWr:
  case AArch64::SBCSXr:
    lift_adc_sbc(opcode);
    break;

  case AArch64::ASRVWr:
  case AArch64::ASRVXr:
    lift_asrv(opcode);
    break;

  case AArch64::SUBWri:
  case AArch64::SUBWrs:
  case AArch64::SUBWrx:
  case AArch64::SUBSWrs:
  case AArch64::SUBSWri:
  case AArch64::SUBSWrx:
  case AArch64::SUBXri:
  case AArch64::SUBXrs:
  case AArch64::SUBXrx:
  case AArch64::SUBXrx64:
  case AArch64::SUBSXrs:
  case AArch64::SUBSXri:
  case AArch64::SUBSXrx:
    lift_sub(opcode);
    break;

  case AArch64::FCSELDrrr:
  case AArch64::FCSELSrrr:
  case AArch64::CSELWr:
  case AArch64::CSELXr:
    lift_csel(opcode);
    break;

  case AArch64::ANDWri:
  case AArch64::ANDWrr:
  case AArch64::ANDWrs:
  case AArch64::ANDSWri:
  case AArch64::ANDSWrr:
  case AArch64::ANDSWrs:
  case AArch64::ANDXri:
  case AArch64::ANDXrr:
  case AArch64::ANDXrs:
  case AArch64::ANDSXri:
  case AArch64::ANDSXrr:
  case AArch64::ANDSXrs:
    lift_and(opcode);
    break;

  case AArch64::MADDWrrr:
  case AArch64::MADDXrrr:
    lift_madd(opcode);
    break;

  case AArch64::UMADDLrrr:
    lift_umadd(opcode);
    break;

  case AArch64::SMADDLrrr:
    lift_smaddl();
    break;

  case AArch64::SMSUBLrrr:
  case AArch64::UMSUBLrrr:
    lift_msubl(opcode);
    break;

  case AArch64::SMULHrr:
  case AArch64::UMULHrr:
    lift_mulh(opcode);
    break;

  case AArch64::MSUBWrrr:
  case AArch64::MSUBXrrr:
    lift_msub();
    break;

  case AArch64::SBFMWri:
  case AArch64::SBFMXri:
    lift_sbfm(opcode);
    break;

  case AArch64::CCMPWi:
  case AArch64::CCMPWr:
  case AArch64::CCMPXi:
  case AArch64::CCMPXr:
    lift_ccmp(opcode);
    break;

  case AArch64::EORWri:
  case AArch64::EORXri:
    lift_eori(opcode);
    break;

  case AArch64::EORWrs:
  case AArch64::EORXrs:
    lift_eorr();
    break;

  case AArch64::CCMNWi:
  case AArch64::CCMNWr:
  case AArch64::CCMNXi:
  case AArch64::CCMNXr:
    lift_ccmn();
    break;

  case AArch64::CSINVWr:
  case AArch64::CSINVXr:
  case AArch64::CSNEGWr:
  case AArch64::CSNEGXr:
    lift_csinv_csneg(opcode);
    break;

  case AArch64::CSINCWr:
  case AArch64::CSINCXr:
    lift_csinc(opcode);
    break;

  case AArch64::MOVZWi:
  case AArch64::MOVZXi:
    lift_movz(opcode);
    break;

  case AArch64::MOVNWi:
  case AArch64::MOVNXi:
    lift_movn();
    break;

  case AArch64::MOVKWi:
  case AArch64::MOVKXi:
    lift_movk(opcode);
    break;

  case AArch64::LSLVWr:
  case AArch64::LSLVXr:
    lift_lslv();
    break;

  case AArch64::LSRVWr:
  case AArch64::LSRVXr:
    lift_lsrv();
    break;

  case AArch64::ORNWrs:
  case AArch64::ORNXrs:
    lift_orn();
    break;

  case AArch64::UBFMWri:
  case AArch64::UBFMXri:
    lift_ubfm(opcode);
    break;

  case AArch64::BFMWri:
  case AArch64::BFMXri:
    lift_bfm(opcode);
    break;

  case AArch64::ORRWri:
  case AArch64::ORRXri:
    lift_orri(opcode);
    break;

  case AArch64::ORRWrs:
  case AArch64::ORRXrs:
    lift_orrr();
    break;

  case AArch64::SDIVWr:
  case AArch64::SDIVXr:
    lift_sdiv(opcode);
    break;

  case AArch64::UDIVWr:
  case AArch64::UDIVXr:
    lift_udiv(opcode);
    break;

  case AArch64::EXTRWrri:
  case AArch64::EXTRXrri:
    lift_extr();
    break;

  case AArch64::RORVWr:
  case AArch64::RORVXr:
    lift_ror();
    break;

  case AArch64::RBITWr:
  case AArch64::RBITXr:
    lift_rbit();
    break;

  case AArch64::REVWr:
  case AArch64::REVXr:
    lift_rev();
    break;

  case AArch64::CLZWr:
  case AArch64::CLZXr:
    lift_clz();
    break;

  case AArch64::EONWrs:
  case AArch64::EONXrs:
  case AArch64::BICWrs:
  case AArch64::BICXrs:
  case AArch64::BICSWrs:
  case AArch64::BICSXrs:
    lift_eon_bic(opcode);
    break;

  case AArch64::REV16Xr:
    lift_rev16(opcode);
    break;

  case AArch64::REV16Wr:
  case AArch64::REV32Xr:
    lift_rev32(opcode);
    break;

  case AArch64::LDRSBXui:
  case AArch64::LDRSBWui:
  case AArch64::LDRSHXui:
  case AArch64::LDRSHWui:
  case AArch64::LDRSWui:
  case AArch64::LDRBBui:
  case AArch64::LDRBui:
  case AArch64::LDRHHui:
  case AArch64::LDRHui:
  case AArch64::LDRWui:
  case AArch64::LDRSui:
  case AArch64::LDRXui:
  case AArch64::LDRDui:
  case AArch64::LDRQui:
    lift_ldr1(opcode);
    break;

  case AArch64::LDURSBWi:
  case AArch64::LDURSBXi:
  case AArch64::LDURSHWi:
  case AArch64::LDURSHXi:
  case AArch64::LDURSWi:
  case AArch64::LDURBi:
  case AArch64::LDURBBi:
  case AArch64::LDURHi:
  case AArch64::LDURHHi:
  case AArch64::LDURSi:
  case AArch64::LDURWi:
  case AArch64::LDURDi:
  case AArch64::LDURXi:
  case AArch64::LDURQi:
    lift_ldu1(opcode);
    break;

  case AArch64::LDRSBWpre:
  case AArch64::LDRSBXpre:
  case AArch64::LDRSHWpre:
  case AArch64::LDRSHXpre:
  case AArch64::LDRSWpre:
  case AArch64::LDRBBpre:
  case AArch64::LDRBpre:
  case AArch64::LDRHHpre:
  case AArch64::LDRHpre:
  case AArch64::LDRWpre:
  case AArch64::LDRSpre:
  case AArch64::LDRXpre:
  case AArch64::LDRDpre:
  case AArch64::LDRQpre:
  case AArch64::LDRSBWpost:
  case AArch64::LDRSBXpost:
  case AArch64::LDRSHWpost:
  case AArch64::LDRSHXpost:
  case AArch64::LDRSWpost:
  case AArch64::LDRBBpost:
  case AArch64::LDRBpost:
  case AArch64::LDRHHpost:
  case AArch64::LDRHpost:
  case AArch64::LDRWpost:
  case AArch64::LDRSpost:
  case AArch64::LDRXpost:
  case AArch64::LDRDpost:
  case AArch64::LDRQpost:
    lift_ldr2(opcode);
    break;

  case AArch64::LDRBBroW:
  case AArch64::LDRBBroX:
  case AArch64::LDRBroW:
  case AArch64::LDRBroX:
  case AArch64::LDRHHroW:
  case AArch64::LDRHHroX:
  case AArch64::LDRHroW:
  case AArch64::LDRHroX:
  case AArch64::LDRWroW:
  case AArch64::LDRWroX:
  case AArch64::LDRSroW:
  case AArch64::LDRSroX:
  case AArch64::LDRXroW:
  case AArch64::LDRXroX:
  case AArch64::LDRDroW:
  case AArch64::LDRDroX:
  case AArch64::LDRQroW:
  case AArch64::LDRQroX:
  case AArch64::LDRSBWroW:
  case AArch64::LDRSBWroX:
  case AArch64::LDRSBXroW:
  case AArch64::LDRSBXroX:
  case AArch64::LDRSHWroW:
  case AArch64::LDRSHWroX:
  case AArch64::LDRSHXroW:
  case AArch64::LDRSHXroX:
  case AArch64::LDRSWroW:
  case AArch64::LDRSWroX:
    lift_ldr3(opcode);
    break;

  case AArch64::LD1i8:
  case AArch64::LD1i16:
  case AArch64::LD1i32:
  case AArch64::LD1i64:
  case AArch64::LD1i8_POST:
  case AArch64::LD1i16_POST:
  case AArch64::LD1i32_POST:
  case AArch64::LD1i64_POST:
    lift_ld1(opcode);
    break;

  case AArch64::LD1Rv8b:
  case AArch64::LD1Rv16b:
  case AArch64::LD1Rv4h:
  case AArch64::LD1Rv8h:
  case AArch64::LD1Rv2s:
  case AArch64::LD1Rv4s:
  case AArch64::LD1Rv1d:
  case AArch64::LD1Rv2d:
  case AArch64::LD1Rv8b_POST:
  case AArch64::LD1Rv16b_POST:
  case AArch64::LD1Rv4h_POST:
  case AArch64::LD1Rv8h_POST:
  case AArch64::LD1Rv2s_POST:
  case AArch64::LD1Rv4s_POST:
  case AArch64::LD1Rv1d_POST:
  case AArch64::LD1Rv2d_POST:
    lift_ld1r(opcode);
    break;

  case AArch64::LD1Onev8b:
  case AArch64::LD1Onev16b:
  case AArch64::LD1Onev4h:
  case AArch64::LD1Onev8h:
  case AArch64::LD1Onev2s:
  case AArch64::LD1Onev4s:
  case AArch64::LD1Onev1d:
  case AArch64::LD1Onev2d:
  case AArch64::LD1Onev8b_POST:
  case AArch64::LD1Onev16b_POST:
  case AArch64::LD1Onev4h_POST:
  case AArch64::LD1Onev8h_POST:
  case AArch64::LD1Onev2s_POST:
  case AArch64::LD1Onev4s_POST:
  case AArch64::LD1Onev1d_POST:
  case AArch64::LD1Onev2d_POST:
  case AArch64::LD1Twov1d:
  case AArch64::LD1Twov2d:
  case AArch64::LD1Threev1d:
  case AArch64::LD1Threev2d:
  case AArch64::LD1Fourv1d:
  case AArch64::LD1Fourv2d:
  case AArch64::LD2Twov8b:
  case AArch64::LD2Twov16b:
  case AArch64::LD2Twov4h:
  case AArch64::LD2Twov8h:
  case AArch64::LD2Twov2s:
  case AArch64::LD2Twov4s:
  case AArch64::LD2Twov2d:
  case AArch64::LD2Twov8b_POST:
  case AArch64::LD2Twov16b_POST:
  case AArch64::LD2Twov4h_POST:
  case AArch64::LD2Twov8h_POST:
  case AArch64::LD2Twov2s_POST:
  case AArch64::LD2Twov4s_POST:
  case AArch64::LD2Twov2d_POST:
  case AArch64::LD3Threev8b:
  case AArch64::LD3Threev16b:
  case AArch64::LD3Threev4h:
  case AArch64::LD3Threev8h:
  case AArch64::LD3Threev2s:
  case AArch64::LD3Threev4s:
  case AArch64::LD3Threev2d:
  case AArch64::LD3Threev8b_POST:
  case AArch64::LD3Threev16b_POST:
  case AArch64::LD3Threev4h_POST:
  case AArch64::LD3Threev8h_POST:
  case AArch64::LD3Threev2s_POST:
  case AArch64::LD3Threev4s_POST:
  case AArch64::LD3Threev2d_POST:
  case AArch64::LD4Fourv8b:
  case AArch64::LD4Fourv16b:
  case AArch64::LD4Fourv4h:
  case AArch64::LD4Fourv8h:
  case AArch64::LD4Fourv2s:
  case AArch64::LD4Fourv4s:
  case AArch64::LD4Fourv2d:
  case AArch64::LD4Fourv8b_POST:
  case AArch64::LD4Fourv16b_POST:
  case AArch64::LD4Fourv4h_POST:
  case AArch64::LD4Fourv8h_POST:
  case AArch64::LD4Fourv2s_POST:
  case AArch64::LD4Fourv4s_POST:
  case AArch64::LD4Fourv2d_POST:
    lift_ld2(opcode);
    break;

  case AArch64::STURBBi:
  case AArch64::STURBi:
  case AArch64::STURHHi:
  case AArch64::STURHi:
  case AArch64::STURWi:
  case AArch64::STURSi:
  case AArch64::STURXi:
  case AArch64::STURDi:
  case AArch64::STURQi:
  case AArch64::STRBBui:
  case AArch64::STRBui:
  case AArch64::STRHHui:
  case AArch64::STRHui:
  case AArch64::STRWui:
  case AArch64::STRSui:
  case AArch64::STRXui:
  case AArch64::STRDui:
  case AArch64::STRQui:
    lift_str_1(opcode);
    break;

  case AArch64::STRBBpre:
  case AArch64::STRBpre:
  case AArch64::STRHHpre:
  case AArch64::STRHpre:
  case AArch64::STRWpre:
  case AArch64::STRSpre:
  case AArch64::STRXpre:
  case AArch64::STRDpre:
  case AArch64::STRQpre:
  case AArch64::STRBBpost:
  case AArch64::STRBpost:
  case AArch64::STRHHpost:
  case AArch64::STRHpost:
  case AArch64::STRWpost:
  case AArch64::STRSpost:
  case AArch64::STRXpost:
  case AArch64::STRDpost:
  case AArch64::STRQpost:
    lift_str_2(opcode);
    break;

  case AArch64::STRBBroW:
  case AArch64::STRBBroX:
  case AArch64::STRBroW:
  case AArch64::STRBroX:
  case AArch64::STRHHroW:
  case AArch64::STRHHroX:
  case AArch64::STRHroW:
  case AArch64::STRHroX:
  case AArch64::STRWroW:
  case AArch64::STRWroX:
  case AArch64::STRSroW:
  case AArch64::STRSroX:
  case AArch64::STRXroW:
  case AArch64::STRXroX:
  case AArch64::STRDroW:
  case AArch64::STRDroX:
  case AArch64::STRQroW:
  case AArch64::STRQroX:
    lift_str_3(opcode);
    break;

  case AArch64::ST1i8:
  case AArch64::ST1i16:
  case AArch64::ST1i32:
  case AArch64::ST1i64:
  case AArch64::ST1i8_POST:
  case AArch64::ST1i16_POST:
  case AArch64::ST1i32_POST:
  case AArch64::ST1i64_POST:
    lift_str_4(opcode);
    break;

  case AArch64::ST1Onev8b:
  case AArch64::ST1Onev16b:
  case AArch64::ST1Onev4h:
  case AArch64::ST1Onev8h:
  case AArch64::ST1Onev2s:
  case AArch64::ST1Onev4s:
  case AArch64::ST1Onev1d:
  case AArch64::ST1Onev2d:
  case AArch64::ST1Onev8b_POST:
  case AArch64::ST1Onev16b_POST:
  case AArch64::ST1Onev4h_POST:
  case AArch64::ST1Onev8h_POST:
  case AArch64::ST1Onev2s_POST:
  case AArch64::ST1Onev4s_POST:
  case AArch64::ST1Onev1d_POST:
  case AArch64::ST1Onev2d_POST:
  case AArch64::ST1Twov1d:
  case AArch64::ST1Threev1d:
  case AArch64::ST1Threev2d:
  case AArch64::ST1Twov2d:
  case AArch64::ST1Fourv1d:
  case AArch64::ST1Fourv2d:
  case AArch64::ST2Twov8b:
  case AArch64::ST2Twov16b:
  case AArch64::ST2Twov4h:
  case AArch64::ST2Twov8h:
  case AArch64::ST2Twov2s:
  case AArch64::ST2Twov4s:
  case AArch64::ST2Twov2d:
  case AArch64::ST2Twov8b_POST:
  case AArch64::ST2Twov16b_POST:
  case AArch64::ST2Twov4h_POST:
  case AArch64::ST2Twov8h_POST:
  case AArch64::ST2Twov2s_POST:
  case AArch64::ST2Twov4s_POST:
  case AArch64::ST2Twov2d_POST:
  case AArch64::ST3Threev8b:
  case AArch64::ST3Threev16b:
  case AArch64::ST3Threev4h:
  case AArch64::ST3Threev8h:
  case AArch64::ST3Threev2s:
  case AArch64::ST3Threev4s:
  case AArch64::ST3Threev2d:
  case AArch64::ST3Threev8b_POST:
  case AArch64::ST3Threev16b_POST:
  case AArch64::ST3Threev4h_POST:
  case AArch64::ST3Threev8h_POST:
  case AArch64::ST3Threev2s_POST:
  case AArch64::ST3Threev4s_POST:
  case AArch64::ST3Threev2d_POST:
  case AArch64::ST4Fourv8b:
  case AArch64::ST4Fourv16b:
  case AArch64::ST4Fourv4h:
  case AArch64::ST4Fourv8h:
  case AArch64::ST4Fourv2s:
  case AArch64::ST4Fourv4s:
  case AArch64::ST4Fourv2d:
  case AArch64::ST4Fourv8b_POST:
  case AArch64::ST4Fourv16b_POST:
  case AArch64::ST4Fourv4h_POST:
  case AArch64::ST4Fourv8h_POST:
  case AArch64::ST4Fourv2s_POST:
  case AArch64::ST4Fourv4s_POST:
  case AArch64::ST4Fourv2d_POST:
    lift_str_5(opcode);
    break;

  case AArch64::LDPSWi:
  case AArch64::LDPWi:
  case AArch64::LDPSi:
  case AArch64::LDPXi:
  case AArch64::LDPDi:
  case AArch64::LDPQi:
    lift_ldp_1(opcode);
    break;

  case AArch64::STPWi:
  case AArch64::STPSi:
  case AArch64::STPXi:
  case AArch64::STPDi:
  case AArch64::STPQi:
    lift_stp_1(opcode);
    break;

  case AArch64::LDPSWpre:
  case AArch64::LDPWpre:
  case AArch64::LDPSpre:
  case AArch64::LDPXpre:
  case AArch64::LDPDpre:
  case AArch64::LDPQpre:
  case AArch64::LDPSWpost:
  case AArch64::LDPWpost:
  case AArch64::LDPSpost:
  case AArch64::LDPXpost:
  case AArch64::LDPDpost:
  case AArch64::LDPQpost:
    lift_ldp_2(opcode);
    break;

  case AArch64::STPWpre:
  case AArch64::STPSpre:
  case AArch64::STPXpre:
  case AArch64::STPDpre:
  case AArch64::STPQpre:
  case AArch64::STPWpost:
  case AArch64::STPSpost:
  case AArch64::STPXpost:
  case AArch64::STPDpost:
  case AArch64::STPQpost:
    lift_stp_2(opcode);
    break;

  case AArch64::ADRP:
    lift_adrp();
    break;

  case AArch64::CBZW:
  case AArch64::CBZX:
    lift_cbz();
    break;

  case AArch64::CBNZW:
  case AArch64::CBNZX:
    lift_cbnz();
    break;

  case AArch64::TBZW:
  case AArch64::TBZX:
  case AArch64::TBNZW:
  case AArch64::TBNZX:
    lift_tbz(opcode);
    break;

  case AArch64::INSvi8lane:
  case AArch64::INSvi16lane:
  case AArch64::INSvi32lane:
  case AArch64::INSvi64lane:
    lift_ins_lane(opcode);
    break;

  case AArch64::INSvi8gpr:
  case AArch64::INSvi16gpr:
  case AArch64::INSvi32gpr:
  case AArch64::INSvi64gpr:
    lift_ins_gpr(opcode);
    break;

  case AArch64::FNEGSr:
  case AArch64::FNEGDr:
    lift_fneg();
    break;

  case AArch64::FNEGv2f32:
  case AArch64::FNEGv4f32:
  case AArch64::FNEGv2f64:
    lift_vec_fneg(opcode);
    break;

  case AArch64::FCVTZUUWHr:
  case AArch64::FCVTZUUWSr:
  case AArch64::FCVTZUUWDr:
  case AArch64::FCVTZUUXHr:
  case AArch64::FCVTZUUXSr:
  case AArch64::FCVTZUUXDr:
  case AArch64::FCVTZSUWHr:
  case AArch64::FCVTZSUWSr:
  case AArch64::FCVTZSUWDr:
  case AArch64::FCVTZSUXHr:
  case AArch64::FCVTZSUXSr:
  case AArch64::FCVTZSUXDr:
    lift_fcvt_1(opcode);
    break;

  case AArch64::FCVTZSv1i32:
  case AArch64::FCVTZSv1i64:
    lift_fcvt_2(opcode);
    break;

  case AArch64::FCVTSHr:
  case AArch64::FCVTDHr:
  case AArch64::FCVTHSr:
  case AArch64::FCVTHDr:
    lift_fcvt_3();
    break;

  case AArch64::FCVTDSr:
  case AArch64::FCVTSDr:
    lift_fcvt_4();
    break;

  case AArch64::FRINTXSr:
  case AArch64::FRINTXDr:
  case AArch64::FRINTASr:
  case AArch64::FRINTADr:
  case AArch64::FRINTMSr:
  case AArch64::FRINTMDr:
  case AArch64::FRINTPSr:
  case AArch64::FRINTPDr:
    lift_frint(opcode);
    break;

  case AArch64::UCVTFUWSri:
  case AArch64::UCVTFUWDri:
  case AArch64::UCVTFUXSri:
  case AArch64::UCVTFUXDri:
  case AArch64::SCVTFUWSri:
  case AArch64::SCVTFUWDri:
  case AArch64::SCVTFUXSri:
  case AArch64::SCVTFUXDri:
    lift_cvtf_1(opcode);
    break;

  case AArch64::SCVTFv1i32:
  case AArch64::SCVTFv1i64:
    lift_cvtf_2(opcode);
    break;

  case AArch64::FMOVSWr:
  case AArch64::FMOVDXr:
  case AArch64::FMOVWSr:
  case AArch64::FMOVXDr:
  case AArch64::FMOVDr:
  case AArch64::FMOVSr:
    lift_fmov_1();
    break;

  case AArch64::FMOVSi:
  case AArch64::FMOVDi:
    lift_fmov_2(opcode);
    break;

  case AArch64::FMOVv2f32_ns:
  case AArch64::FMOVv4f32_ns:
  case AArch64::FMOVv2f64_ns:
    lift_fmov_3(opcode);
    break;

  case AArch64::FABSSr:
  case AArch64::FABSDr:
    lift_fabs();
    break;

  case AArch64::FMINSrr:
  case AArch64::FMINNMSrr:
  case AArch64::FMAXSrr:
  case AArch64::FMAXNMSrr:
  case AArch64::FMINDrr:
  case AArch64::FMINNMDrr:
  case AArch64::FMAXDrr:
  case AArch64::FMAXNMDrr:
    lift_fminmax(opcode);
    break;

  case AArch64::FMULSrr:
  case AArch64::FMULDrr:
  case AArch64::FNMULSrr:
  case AArch64::FNMULDrr:
  case AArch64::FDIVSrr:
  case AArch64::FDIVDrr:
  case AArch64::FADDSrr:
  case AArch64::FADDDrr:
  case AArch64::FSUBSrr:
  case AArch64::FSUBDrr:
    lift_fpbinop(opcode);
    break;

  case AArch64::FMULv1i32_indexed:
  case AArch64::FMULv1i64_indexed:
    lift_fmul_idx(opcode);
    break;

  case AArch64::FMLAv1i32_indexed:
  case AArch64::FMLAv1i64_indexed:
  case AArch64::FMLSv1i32_indexed:
  case AArch64::FMLSv1i64_indexed:
    lift_fmla_mls(opcode);
    break;

  case AArch64::FMADDSrrr:
  case AArch64::FMADDDrrr:
  case AArch64::FMSUBSrrr:
  case AArch64::FMSUBDrrr:
  case AArch64::FNMADDSrrr:
  case AArch64::FNMADDDrrr:
  case AArch64::FNMSUBSrrr:
  case AArch64::FNMSUBDrrr:
    lift_fnm(opcode);
    break;

  case AArch64::FSQRTSr:
  case AArch64::FSQRTDr:
    lift_fsqrt();
    break;

  case AArch64::FADDv2f32:
  case AArch64::FADDv4f32:
  case AArch64::FADDv2f64:
  case AArch64::FSUBv2f32:
  case AArch64::FSUBv4f32:
  case AArch64::FSUBv2f64:
  case AArch64::FMULv2f32:
  case AArch64::FMULv4f32:
  case AArch64::FMULv2f64:
    lift_vec_fpbinop(opcode);
    break;

  case AArch64::FCMPSri:
  case AArch64::FCMPDri:
  case AArch64::FCMPESri:
  case AArch64::FCMPEDri:
  case AArch64::FCMPSrr:
  case AArch64::FCMPDrr:
  case AArch64::FCMPESrr:
  case AArch64::FCMPEDrr:
    lift_fcmp(opcode);
    break;

  case AArch64::FCCMPSrr:
  case AArch64::FCCMPDrr:
    lift_fccmp();
    break;

  case AArch64::SMOVvi8to32_idx0:
  case AArch64::SMOVvi8to32:
  case AArch64::SMOVvi16to32_idx0:
  case AArch64::SMOVvi16to32:
  case AArch64::SMOVvi8to64_idx0:
  case AArch64::SMOVvi8to64:
  case AArch64::SMOVvi16to64_idx0:
  case AArch64::SMOVvi16to64:
  case AArch64::SMOVvi32to64_idx0:
  case AArch64::SMOVvi32to64:
    lift_smov_vi(opcode);
    break;

  case AArch64::UMOVvi8:
  case AArch64::UMOVvi8_idx0:
  case AArch64::UMOVvi16:
  case AArch64::UMOVvi16_idx0:
  case AArch64::UMOVvi32:
  case AArch64::UMOVvi32_idx0:
  case AArch64::UMOVvi64:
    lift_umov_vi(opcode);
    break;

  case AArch64::MVNIv8i16:
  case AArch64::MVNIv4i32:
  case AArch64::MVNIv4i16:
  case AArch64::MVNIv2i32:
    lift_mvni(opcode);
    break;

  case AArch64::MVNIv2s_msl:
  case AArch64::MVNIv4s_msl:
    lift_mvni_msl(opcode);
    break;

  case AArch64::MOVIv2s_msl:
  case AArch64::MOVIv4s_msl:
    lift_movi_msl(opcode);
    break;

  case AArch64::MOVID:
  case AArch64::MOVIv2d_ns:
    lift_movi_1();
    break;

  case AArch64::MOVIv8b_ns:
    lift_movi_2();
    break;

  case AArch64::MOVIv16b_ns:
    lift_movi_3();
    break;

  case AArch64::MOVIv4i16:
    lift_movi_4();
    break;

  case AArch64::MOVIv8i16:
    lift_movi_5();
    break;

  case AArch64::MOVIv2i32:
    lift_movi_6();
    break;

  case AArch64::MOVIv4i32:
    lift_movi_7();
    break;

  case AArch64::EXTv8i8:
    lift_ext_1();
    break;

  case AArch64::EXTv16i8:
    lift_ext_2();
    break;

  case AArch64::REV64v4i32:
    lift_rev64_1();
    break;

  case AArch64::REV64v2i32:
    lift_rev64_2();
    break;

  case AArch64::REV64v4i16:
    lift_rev64_3();
    break;

  case AArch64::REV64v8i8:
    lift_rev64_4();
    break;

  case AArch64::REV64v8i16:
    lift_rev64_5();
    break;

  case AArch64::REV64v16i8:
    lift_rev64_6();
    break;

  case AArch64::REV32v4i16:
    lift_rev32_1();
    break;

  case AArch64::REV32v8i8:
    lift_rev32_2();
    break;

  case AArch64::REV32v8i16:
    lift_rev32_3();
    break;

  case AArch64::REV32v16i8:
    lift_rev32_4();
    break;

  case AArch64::REV16v8i8:
    lift_rev16_1();
    break;

  case AArch64::REV16v16i8:
    lift_rev16_2();
    break;

  case AArch64::DUPi8:
    lift_dup8();
    break;

  case AArch64::DUPi16:
    lift_dup16();
    break;

  case AArch64::DUPi32:
    lift_dup32();
    break;

  case AArch64::DUPi64:
    lift_dup64();
    break;

  case AArch64::DUPv8i8gpr:
    lift_dup88();
    break;

  case AArch64::DUPv16i8gpr:
    lift_dup168();
    break;

  case AArch64::DUPv8i16gpr:
    lift_dup816();
    break;

  case AArch64::DUPv4i16gpr:
    lift_dup416();
    break;

  case AArch64::DUPv4i32gpr:
    lift_dup432();
    break;

  case AArch64::DUPv2i32gpr:
    lift_dup232();
    break;

  case AArch64::DUPv2i64gpr:
    lift_dup264();
    break;

  case AArch64::DUPv2i32lane:
    lift_dup232lane();
    break;

  case AArch64::DUPv2i64lane:
    lift_dup264lane();
    break;

  case AArch64::DUPv4i16lane:
    lift_dup416lane();
    break;

  case AArch64::DUPv4i32lane:
    lift_dup432lane();
    break;

  case AArch64::DUPv8i8lane:
    lift_dup88lane();
    break;

  case AArch64::DUPv8i16lane:
    lift_dup816lane();
    break;

  case AArch64::DUPv16i8lane:
    lift_dup168lane();
    break;

  case AArch64::BIFv8i8:
  case AArch64::BIFv16i8:
    lift_bif1();
    break;

  case AArch64::BITv16i8:
  case AArch64::BITv8i8:
    lift_bif2();
    break;

  case AArch64::BSLv8i8:
  case AArch64::BSLv16i8:
    lift_bif3();
    break;

  case AArch64::FCMLEv2i64rz:
  case AArch64::FCMLEv4i32rz:
  case AArch64::FCMLEv2i32rz:
  case AArch64::FCMLTv2i32rz:
  case AArch64::FCMLTv2i64rz:
  case AArch64::FCMLTv4i32rz:
  case AArch64::FCMEQv2i32rz:
  case AArch64::FCMEQv2i64rz:
  case AArch64::FCMEQv4i32rz:
  case AArch64::FCMGTv4i32rz:
  case AArch64::FCMGTv2i64rz:
  case AArch64::FCMGTv2i32rz:
  case AArch64::FCMGEv2i32rz:
  case AArch64::FCMGEv4i32rz:
  case AArch64::FCMGEv2i64rz:
  case AArch64::FCMEQv2f32:
  case AArch64::FCMEQv4f32:
  case AArch64::FCMEQv2f64:
  case AArch64::FCMGTv2f32:
  case AArch64::FCMGTv4f32:
  case AArch64::FCMGTv2f64:
  case AArch64::FCMGEv2f32:
  case AArch64::FCMGEv4f32:
  case AArch64::FCMGEv2f64:
    lift_fcm(opcode);
    break;

  case AArch64::CMGTv4i16rz:
  case AArch64::CMEQv1i64rz:
  case AArch64::CMGTv8i8rz:
  case AArch64::CMLEv4i16rz:
  case AArch64::CMGEv4i16rz:
  case AArch64::CMLEv8i8rz:
  case AArch64::CMLTv1i64rz:
  case AArch64::CMGTv1i64rz:
  case AArch64::CMGTv2i32rz:
  case AArch64::CMGEv8i8rz:
  case AArch64::CMLEv1i64rz:
  case AArch64::CMLEv2i32rz:
  case AArch64::CMGEv2i32rz:
  case AArch64::CMLEv8i16rz:
  case AArch64::CMLEv16i8rz:
  case AArch64::CMGTv16i8rz:
  case AArch64::CMGTv8i16rz:
  case AArch64::CMGTv2i64rz:
  case AArch64::CMGEv8i16rz:
  case AArch64::CMLEv4i32rz:
  case AArch64::CMGEv2i64rz:
  case AArch64::CMLEv2i64rz:
  case AArch64::CMGEv16i8rz:
  case AArch64::CMGEv4i32rz:
  case AArch64::CMEQv1i64:
  case AArch64::CMEQv16i8:
  case AArch64::CMEQv16i8rz:
  case AArch64::CMEQv2i32:
  case AArch64::CMEQv2i32rz:
  case AArch64::CMEQv2i64:
  case AArch64::CMEQv2i64rz:
  case AArch64::CMEQv4i16:
  case AArch64::CMEQv4i16rz:
  case AArch64::CMEQv4i32:
  case AArch64::CMEQv4i32rz:
  case AArch64::CMEQv8i16:
  case AArch64::CMEQv8i16rz:
  case AArch64::CMEQv8i8:
  case AArch64::CMEQv8i8rz:
  case AArch64::CMGEv16i8:
  case AArch64::CMGEv2i32:
  case AArch64::CMGEv1i64:
  case AArch64::CMGEv2i64:
  case AArch64::CMGEv4i16:
  case AArch64::CMGEv4i32:
  case AArch64::CMGEv8i16:
  case AArch64::CMGEv8i8:
  case AArch64::CMGTv16i8:
  case AArch64::CMGTv2i32:
  case AArch64::CMGTv2i64:
  case AArch64::CMGTv4i16:
  case AArch64::CMGTv4i32:
  case AArch64::CMGTv4i32rz:
  case AArch64::CMGTv8i16:
  case AArch64::CMGTv8i8:
  case AArch64::CMHIv16i8:
  case AArch64::CMGTv1i64:
  case AArch64::CMHIv1i64:
  case AArch64::CMHIv2i32:
  case AArch64::CMHIv2i64:
  case AArch64::CMHIv4i16:
  case AArch64::CMHIv4i32:
  case AArch64::CMHIv8i16:
  case AArch64::CMHIv8i8:
  case AArch64::CMHSv16i8:
  case AArch64::CMHSv1i64:
  case AArch64::CMHSv2i32:
  case AArch64::CMHSv2i64:
  case AArch64::CMHSv4i16:
  case AArch64::CMHSv4i32:
  case AArch64::CMHSv8i16:
  case AArch64::CMHSv8i8:
  case AArch64::CMLTv16i8rz:
  case AArch64::CMLTv2i32rz:
  case AArch64::CMLTv2i64rz:
  case AArch64::CMLTv4i16rz:
  case AArch64::CMLTv4i32rz:
  case AArch64::CMLTv8i16rz:
  case AArch64::CMLTv8i8rz:
  case AArch64::CMTSTv4i16:
  case AArch64::CMTSTv2i32:
  case AArch64::CMTSTv16i8:
  case AArch64::CMTSTv2i64:
  case AArch64::CMTSTv4i32:
  case AArch64::CMTSTv8i16:
  case AArch64::CMTSTv8i8:
    lift_cm(opcode);
    break;

  case AArch64::TBLv8i8One:
  case AArch64::TBLv8i8Two:
  case AArch64::TBLv8i8Three:
  case AArch64::TBLv8i8Four:
  case AArch64::TBLv16i8One:
  case AArch64::TBLv16i8Two:
  case AArch64::TBLv16i8Three:
  case AArch64::TBLv16i8Four:
    lift_tbl(opcode);
    break;

  case AArch64::RADDHNv2i64_v2i32:
  case AArch64::RADDHNv2i64_v4i32:
  case AArch64::RADDHNv4i32_v4i16:
  case AArch64::RADDHNv4i32_v8i16:
  case AArch64::RADDHNv8i16_v8i8:
  case AArch64::RADDHNv8i16_v16i8:
  case AArch64::ADDHNv2i64_v2i32:
  case AArch64::ADDHNv2i64_v4i32:
  case AArch64::ADDHNv4i32_v4i16:
  case AArch64::ADDHNv4i32_v8i16:
  case AArch64::ADDHNv8i16_v8i8:
  case AArch64::ADDHNv8i16_v16i8:
  case AArch64::RSUBHNv2i64_v2i32:
  case AArch64::RSUBHNv2i64_v4i32:
  case AArch64::RSUBHNv4i32_v4i16:
  case AArch64::RSUBHNv4i32_v8i16:
  case AArch64::RSUBHNv8i16_v8i8:
  case AArch64::RSUBHNv8i16_v16i8:
  case AArch64::SUBHNv2i64_v2i32:
  case AArch64::SUBHNv2i64_v4i32:
  case AArch64::SUBHNv4i32_v4i16:
  case AArch64::SUBHNv4i32_v8i16:
  case AArch64::SUBHNv8i16_v8i8:
  case AArch64::SUBHNv8i16_v16i8:
    lift_xhn(opcode);
    break;

  case AArch64::ORNv8i8:
  case AArch64::ORNv16i8:
  case AArch64::SMINv8i8:
  case AArch64::SMINv4i16:
  case AArch64::SMINv2i32:
  case AArch64::SMINv16i8:
  case AArch64::SMINv8i16:
  case AArch64::SMINv4i32:
  case AArch64::SMAXv8i8:
  case AArch64::SMAXv4i16:
  case AArch64::SMAXv2i32:
  case AArch64::SMAXv16i8:
  case AArch64::SMAXv8i16:
  case AArch64::SMAXv4i32:
  case AArch64::UMINv8i8:
  case AArch64::UMINv4i16:
  case AArch64::UMINv2i32:
  case AArch64::UMINv16i8:
  case AArch64::UMINv8i16:
  case AArch64::UMINv4i32:
  case AArch64::UMAXv8i8:
  case AArch64::UMAXv4i16:
  case AArch64::UMAXv2i32:
  case AArch64::UMAXv16i8:
  case AArch64::UMAXv8i16:
  case AArch64::UMAXv4i32:
  case AArch64::SMINPv8i8:
  case AArch64::SMINPv4i16:
  case AArch64::SMINPv2i32:
  case AArch64::SMINPv16i8:
  case AArch64::SMINPv8i16:
  case AArch64::SMINPv4i32:
  case AArch64::SMAXPv8i8:
  case AArch64::SMAXPv4i16:
  case AArch64::SMAXPv2i32:
  case AArch64::SMAXPv16i8:
  case AArch64::SMAXPv8i16:
  case AArch64::SMAXPv4i32:
  case AArch64::UMINPv8i8:
  case AArch64::UMINPv4i16:
  case AArch64::UMINPv2i32:
  case AArch64::UMINPv16i8:
  case AArch64::UMINPv8i16:
  case AArch64::UMINPv4i32:
  case AArch64::UMAXPv8i8:
  case AArch64::UMAXPv4i16:
  case AArch64::UMAXPv2i32:
  case AArch64::UMAXPv16i8:
  case AArch64::UMAXPv8i16:
  case AArch64::UMAXPv4i32:
  case AArch64::SMULLv8i8_v8i16:
  case AArch64::SMULLv2i32_v2i64:
  case AArch64::SMULLv4i16_v4i32:
  case AArch64::SMULLv8i16_v4i32:
  case AArch64::SMULLv16i8_v8i16:
  case AArch64::SMULLv4i32_v2i64:
  case AArch64::USHRv8i8_shift:
  case AArch64::USHRv4i16_shift:
  case AArch64::USHRv2i32_shift:
  case AArch64::USHRv16i8_shift:
  case AArch64::USHRv8i16_shift:
  case AArch64::USHRv4i32_shift:
  case AArch64::USHRd:
  case AArch64::USHRv2i64_shift:
  case AArch64::MULv2i32:
  case AArch64::MULv8i8:
  case AArch64::MULv4i16:
  case AArch64::MULv16i8:
  case AArch64::MULv8i16:
  case AArch64::MULv4i32:
  case AArch64::SSHRv8i8_shift:
  case AArch64::SSHRv16i8_shift:
  case AArch64::SSHRv4i16_shift:
  case AArch64::SSHRv8i16_shift:
  case AArch64::SSHRv2i32_shift:
  case AArch64::SSHRv4i32_shift:
  case AArch64::SSHRd:
  case AArch64::SSHRv2i64_shift:
  case AArch64::SHLv16i8_shift:
  case AArch64::SHLv8i16_shift:
  case AArch64::SHLv4i32_shift:
  case AArch64::SHLv2i64_shift:
  case AArch64::SHLv8i8_shift:
  case AArch64::SHLv4i16_shift:
  case AArch64::SHLd:
  case AArch64::SHLv2i32_shift:
  case AArch64::BICv4i16:
  case AArch64::BICv8i8:
  case AArch64::BICv2i32:
  case AArch64::BICv8i16:
  case AArch64::BICv4i32:
  case AArch64::BICv16i8:
  case AArch64::ADDv2i32:
  case AArch64::ADDv1i64:
  case AArch64::ADDv2i64:
  case AArch64::ADDv4i16:
  case AArch64::ADDv4i32:
  case AArch64::ADDv8i8:
  case AArch64::ADDv8i16:
  case AArch64::ADDv16i8:
  case AArch64::UADDLv8i8_v8i16:
  case AArch64::UADDLv16i8_v8i16:
  case AArch64::UADDLv4i16_v4i32:
  case AArch64::UADDLv8i16_v4i32:
  case AArch64::UADDLv2i32_v2i64:
  case AArch64::UADDLv4i32_v2i64:
  case AArch64::UADDWv8i8_v8i16:
  case AArch64::UADDWv16i8_v8i16:
  case AArch64::UADDWv4i16_v4i32:
  case AArch64::UADDWv8i16_v4i32:
  case AArch64::UADDWv2i32_v2i64:
  case AArch64::UADDWv4i32_v2i64:
  case AArch64::SADDLv8i8_v8i16:
  case AArch64::SADDLv16i8_v8i16:
  case AArch64::SADDLv4i16_v4i32:
  case AArch64::SADDLv8i16_v4i32:
  case AArch64::SADDLv2i32_v2i64:
  case AArch64::SADDLv4i32_v2i64:
  case AArch64::SADDWv8i8_v8i16:
  case AArch64::SADDWv16i8_v8i16:
  case AArch64::SADDWv4i16_v4i32:
  case AArch64::SADDWv8i16_v4i32:
  case AArch64::SADDWv2i32_v2i64:
  case AArch64::SADDWv4i32_v2i64:
  case AArch64::USUBLv8i8_v8i16:
  case AArch64::USUBLv16i8_v8i16:
  case AArch64::USUBLv4i16_v4i32:
  case AArch64::USUBLv8i16_v4i32:
  case AArch64::USUBLv2i32_v2i64:
  case AArch64::USUBLv4i32_v2i64:
  case AArch64::USUBWv8i8_v8i16:
  case AArch64::USUBWv16i8_v8i16:
  case AArch64::USUBWv4i16_v4i32:
  case AArch64::USUBWv8i16_v4i32:
  case AArch64::USUBWv2i32_v2i64:
  case AArch64::USUBWv4i32_v2i64:
  case AArch64::SSUBLv8i8_v8i16:
  case AArch64::SSUBLv16i8_v8i16:
  case AArch64::SSUBLv4i16_v4i32:
  case AArch64::SSUBLv8i16_v4i32:
  case AArch64::SSUBLv2i32_v2i64:
  case AArch64::SSUBLv4i32_v2i64:
  case AArch64::SSUBWv8i8_v8i16:
  case AArch64::SSUBWv16i8_v8i16:
  case AArch64::SSUBWv4i16_v4i32:
  case AArch64::SSUBWv8i16_v4i32:
  case AArch64::SSUBWv2i32_v2i64:
  case AArch64::SSUBWv4i32_v2i64:
  case AArch64::SUBv1i64:
  case AArch64::SUBv2i32:
  case AArch64::SUBv2i64:
  case AArch64::SUBv4i16:
  case AArch64::SUBv4i32:
  case AArch64::SUBv8i8:
  case AArch64::SUBv8i16:
  case AArch64::SUBv16i8:
  case AArch64::EORv8i8:
  case AArch64::EORv16i8:
  case AArch64::ANDv8i8:
  case AArch64::ANDv16i8:
  case AArch64::ORRv8i8:
  case AArch64::ORRv16i8:
  case AArch64::ORRv2i32:
  case AArch64::ORRv4i16:
  case AArch64::ORRv8i16:
  case AArch64::ORRv4i32:
  case AArch64::UMULLv2i32_v2i64:
  case AArch64::UMULLv8i8_v8i16:
  case AArch64::UMULLv4i16_v4i32:
  case AArch64::UMULLv16i8_v8i16:
  case AArch64::UMULLv8i16_v4i32:
  case AArch64::UMULLv4i32_v2i64:
  case AArch64::USHLv1i64:
  case AArch64::USHLv4i16:
  case AArch64::USHLv16i8:
  case AArch64::USHLv8i16:
  case AArch64::USHLv2i32:
  case AArch64::USHLv4i32:
  case AArch64::USHLv8i8:
  case AArch64::USHLv2i64:
  case AArch64::SSHLv1i64:
  case AArch64::SSHLv4i16:
  case AArch64::SSHLv16i8:
  case AArch64::SSHLv8i16:
  case AArch64::SSHLv2i32:
  case AArch64::SSHLv4i32:
  case AArch64::SSHLv8i8:
  case AArch64::SSHLv2i64:
  case AArch64::SSHLLv8i8_shift:
  case AArch64::SSHLLv4i16_shift:
  case AArch64::SSHLLv2i32_shift:
  case AArch64::SSHLLv4i32_shift:
  case AArch64::SSHLLv8i16_shift:
  case AArch64::SSHLLv16i8_shift:
  case AArch64::USHLLv8i8_shift:
  case AArch64::USHLLv4i32_shift:
  case AArch64::USHLLv8i16_shift:
  case AArch64::USHLLv16i8_shift:
  case AArch64::USHLLv4i16_shift:
  case AArch64::USHLLv2i32_shift:
    lift_vec_binop(opcode);
    break;

  case AArch64::ADDPv2i64p:
    lift_addp();
    break;

  case AArch64::UZP1v8i8:
  case AArch64::UZP1v4i16:
  case AArch64::UZP1v16i8:
  case AArch64::UZP1v8i16:
  case AArch64::UZP1v4i32:
  case AArch64::UZP2v8i8:
  case AArch64::UZP2v4i16:
  case AArch64::UZP2v8i16:
  case AArch64::UZP2v16i8:
  case AArch64::UZP2v4i32:
    lift_uzp(opcode);
    break;

  case AArch64::MULv8i16_indexed:
  case AArch64::MULv4i32_indexed:
  case AArch64::MULv2i32_indexed:
  case AArch64::MULv4i16_indexed:
    lift_vec_mul(opcode);
    break;

  case AArch64::MLSv2i32_indexed:
  case AArch64::MLSv4i16_indexed:
  case AArch64::MLSv8i16_indexed:
  case AArch64::MLSv4i32_indexed:
    lift_vec_mls(opcode);
    break;

  case AArch64::MLAv2i32_indexed:
  case AArch64::MLAv4i16_indexed:
  case AArch64::MLAv8i16_indexed:
  case AArch64::MLAv4i32_indexed:
    lift_vec_mla(opcode);
    break;

  case AArch64::TRN1v16i8:
  case AArch64::TRN1v8i16:
  case AArch64::TRN1v4i32:
  case AArch64::TRN1v4i16:
  case AArch64::TRN1v8i8:
  case AArch64::TRN2v8i8:
  case AArch64::TRN2v4i16:
  case AArch64::TRN2v16i8:
  case AArch64::TRN2v8i16:
  case AArch64::TRN2v4i32:
    lift_trn(opcode);
    break;

  case AArch64::SMINVv8i8v:
  case AArch64::UMINVv8i8v:
  case AArch64::SMAXVv8i8v:
  case AArch64::UMAXVv8i8v:
  case AArch64::SMINVv4i16v:
  case AArch64::UMINVv4i16v:
  case AArch64::SMAXVv4i16v:
  case AArch64::UMAXVv4i16v:
  case AArch64::SMAXVv4i32v:
  case AArch64::UMAXVv4i32v:
  case AArch64::SMINVv4i32v:
  case AArch64::UMINVv4i32v:
  case AArch64::UMINVv8i16v:
  case AArch64::SMINVv8i16v:
  case AArch64::UMAXVv8i16v:
  case AArch64::SMAXVv8i16v:
  case AArch64::SMINVv16i8v:
  case AArch64::UMINVv16i8v:
  case AArch64::SMAXVv16i8v:
  case AArch64::UMAXVv16i8v:
    lift_vec_minmax(opcode);
    break;

  case AArch64::SHLLv8i8:
  case AArch64::SHLLv16i8:
  case AArch64::SHLLv4i16:
  case AArch64::SHLLv8i16:
  case AArch64::SHLLv2i32:
  case AArch64::SHLLv4i32:
    lift_shll(opcode);
    break;

  case AArch64::SHRNv8i8_shift:
  case AArch64::SHRNv16i8_shift:
  case AArch64::SHRNv4i16_shift:
  case AArch64::SHRNv8i16_shift:
  case AArch64::SHRNv2i32_shift:
  case AArch64::SHRNv4i32_shift:
  case AArch64::RSHRNv8i8_shift:
  case AArch64::RSHRNv16i8_shift:
  case AArch64::RSHRNv4i16_shift:
  case AArch64::RSHRNv8i16_shift:
  case AArch64::RSHRNv2i32_shift:
  case AArch64::RSHRNv4i32_shift:
    lift_shrn(opcode);
    break;

  case AArch64::SLIv8i8_shift:
  case AArch64::SLIv16i8_shift:
  case AArch64::SLIv4i16_shift:
  case AArch64::SLIv8i16_shift:
  case AArch64::SLIv2i32_shift:
  case AArch64::SLIv4i32_shift:
  case AArch64::SLId:
  case AArch64::SLIv2i64_shift:
  case AArch64::SRIv8i8_shift:
  case AArch64::SRIv16i8_shift:
  case AArch64::SRIv4i16_shift:
  case AArch64::SRIv8i16_shift:
  case AArch64::SRIv2i32_shift:
  case AArch64::SRIv4i32_shift:
  case AArch64::SRId:
  case AArch64::SRIv2i64_shift:
    lift_sli_sri(opcode);
    break;

  case AArch64::MLSv8i8:
  case AArch64::MLSv2i32:
  case AArch64::MLSv4i16:
  case AArch64::MLSv16i8:
  case AArch64::MLSv8i16:
  case AArch64::MLSv4i32:
    lift_mls(opcode);
    break;

  case AArch64::MLAv8i8:
  case AArch64::MLAv2i32:
  case AArch64::MLAv4i16:
  case AArch64::MLAv16i8:
  case AArch64::MLAv8i16:
  case AArch64::MLAv4i32:
    lift_mla(opcode);
    break;

  case AArch64::UABALv8i8_v8i16:
  case AArch64::UABALv16i8_v8i16:
  case AArch64::UABALv4i16_v4i32:
  case AArch64::UABALv8i16_v4i32:
  case AArch64::UABALv2i32_v2i64:
  case AArch64::UABALv4i32_v2i64:
  case AArch64::UABDLv8i8_v8i16:
  case AArch64::UABDLv16i8_v8i16:
  case AArch64::UABDLv4i16_v4i32:
  case AArch64::UABDLv8i16_v4i32:
  case AArch64::UABDLv2i32_v2i64:
  case AArch64::UABDLv4i32_v2i64:
  case AArch64::SABALv8i8_v8i16:
  case AArch64::SABALv16i8_v8i16:
  case AArch64::SABALv4i16_v4i32:
  case AArch64::SABALv8i16_v4i32:
  case AArch64::SABALv2i32_v2i64:
  case AArch64::SABALv4i32_v2i64:
  case AArch64::SABDLv8i8_v8i16:
  case AArch64::SABDLv16i8_v8i16:
  case AArch64::SABDLv4i16_v4i32:
  case AArch64::SABDLv8i16_v4i32:
  case AArch64::SABDLv2i32_v2i64:
  case AArch64::SABDLv4i32_v2i64:
    lift_abal_abdl(opcode);
    break;

  case AArch64::UMLALv4i16_indexed:
  case AArch64::UMLALv8i16_indexed:
  case AArch64::UMLALv2i32_indexed:
  case AArch64::UMLALv4i32_indexed:
  case AArch64::UMLSLv4i16_indexed:
  case AArch64::UMLSLv8i16_indexed:
  case AArch64::UMLSLv2i32_indexed:
  case AArch64::UMLSLv4i32_indexed:
  case AArch64::SMLALv4i16_indexed:
  case AArch64::SMLALv8i16_indexed:
  case AArch64::SMLALv2i32_indexed:
  case AArch64::SMLALv4i32_indexed:
  case AArch64::SMLSLv4i16_indexed:
  case AArch64::SMLSLv8i16_indexed:
  case AArch64::SMLSLv2i32_indexed:
  case AArch64::SMLSLv4i32_indexed:
    lift_mlal_mlsl_idx(opcode);
    break;

  case AArch64::UMLALv8i8_v8i16:
  case AArch64::UMLALv16i8_v8i16:
  case AArch64::UMLALv4i16_v4i32:
  case AArch64::UMLALv8i16_v4i32:
  case AArch64::UMLALv2i32_v2i64:
  case AArch64::UMLALv4i32_v2i64:
  case AArch64::UMLSLv8i8_v8i16:
  case AArch64::UMLSLv16i8_v8i16:
  case AArch64::UMLSLv4i16_v4i32:
  case AArch64::UMLSLv8i16_v4i32:
  case AArch64::UMLSLv2i32_v2i64:
  case AArch64::UMLSLv4i32_v2i64:
  case AArch64::SMLALv8i8_v8i16:
  case AArch64::SMLALv16i8_v8i16:
  case AArch64::SMLALv4i16_v4i32:
  case AArch64::SMLALv8i16_v4i32:
  case AArch64::SMLALv2i32_v2i64:
  case AArch64::SMLALv4i32_v2i64:
  case AArch64::SMLSLv8i8_v8i16:
  case AArch64::SMLSLv16i8_v8i16:
  case AArch64::SMLSLv4i16_v4i32:
  case AArch64::SMLSLv8i16_v4i32:
  case AArch64::SMLSLv2i32_v2i64:
  case AArch64::SMLSLv4i32_v2i64:
    lift_mlal_mlsl(opcode);
    break;

  case AArch64::UABAv8i8:
  case AArch64::UABAv16i8:
  case AArch64::UABAv4i16:
  case AArch64::UABAv8i16:
  case AArch64::UABAv2i32:
  case AArch64::UABAv4i32:
  case AArch64::UABDv8i8:
  case AArch64::UABDv16i8:
  case AArch64::UABDv4i16:
  case AArch64::UABDv8i16:
  case AArch64::UABDv2i32:
  case AArch64::UABDv4i32:
  case AArch64::SABAv8i8:
  case AArch64::SABAv16i8:
  case AArch64::SABAv4i16:
  case AArch64::SABAv8i16:
  case AArch64::SABAv2i32:
  case AArch64::SABAv4i32:
  case AArch64::SABDv8i8:
  case AArch64::SABDv16i8:
  case AArch64::SABDv4i16:
  case AArch64::SABDv8i16:
  case AArch64::SABDv2i32:
  case AArch64::SABDv4i32:
    lift_aba_abd(opcode);
    break;

  case AArch64::SSRAv8i8_shift:
  case AArch64::SSRAv16i8_shift:
  case AArch64::SSRAv4i16_shift:
  case AArch64::SSRAv8i16_shift:
  case AArch64::SSRAv2i32_shift:
  case AArch64::SSRAv4i32_shift:
  case AArch64::SSRAd:
  case AArch64::SSRAv2i64_shift:
    lift_ssra(opcode);
    break;

  case AArch64::USRAv8i8_shift:
  case AArch64::USRAv16i8_shift:
  case AArch64::USRAv4i16_shift:
  case AArch64::USRAv8i16_shift:
  case AArch64::USRAv2i32_shift:
  case AArch64::USRAv4i32_shift:
  case AArch64::USRAv2i64_shift:
  case AArch64::USRAd:
    lift_usra(opcode);
    break;

  case AArch64::ZIP1v4i16:
  case AArch64::ZIP1v2i32:
  case AArch64::ZIP1v8i8:
  case AArch64::ZIP1v8i16:
  case AArch64::ZIP1v16i8:
  case AArch64::ZIP1v2i64:
  case AArch64::ZIP1v4i32:
    lift_zip1(opcode);
    break;

  case AArch64::ZIP2v4i16:
  case AArch64::ZIP2v2i32:
  case AArch64::ZIP2v8i8:
  case AArch64::ZIP2v8i16:
  case AArch64::ZIP2v2i64:
  case AArch64::ZIP2v16i8:
  case AArch64::ZIP2v4i32:
    lift_zip2(opcode);
    break;

  case AArch64::ADDPv8i8:
  case AArch64::ADDPv4i16:
  case AArch64::ADDPv2i32:
  case AArch64::ADDPv16i8:
  case AArch64::ADDPv4i32:
  case AArch64::ADDPv8i16:
  case AArch64::ADDPv2i64:
    lift_addp(opcode);
    break;

  case AArch64::UQADDv1i32:
  case AArch64::UQADDv1i64:
  case AArch64::UQADDv8i8:
  case AArch64::UQADDv4i16:
  case AArch64::UQADDv2i32:
  case AArch64::UQADDv2i64:
  case AArch64::UQADDv4i32:
  case AArch64::UQADDv16i8:
  case AArch64::UQADDv8i16:
    lift_uqadd(opcode);
    break;

  case AArch64::UQSUBv1i32:
  case AArch64::UQSUBv1i64:
  case AArch64::UQSUBv8i8:
  case AArch64::UQSUBv4i16:
  case AArch64::UQSUBv2i32:
  case AArch64::UQSUBv2i64:
  case AArch64::UQSUBv4i32:
  case AArch64::UQSUBv16i8:
  case AArch64::UQSUBv8i16:
    lift_uqsub(opcode);
    break;

  case AArch64::SQADDv1i32:
  case AArch64::SQADDv1i64:
  case AArch64::SQADDv8i8:
  case AArch64::SQADDv4i16:
  case AArch64::SQADDv2i32:
  case AArch64::SQADDv2i64:
  case AArch64::SQADDv4i32:
  case AArch64::SQADDv16i8:
  case AArch64::SQADDv8i16:
    lift_sqadd(opcode);
    break;

  case AArch64::SQSUBv1i32:
  case AArch64::SQSUBv1i64:
  case AArch64::SQSUBv8i8:
  case AArch64::SQSUBv4i16:
  case AArch64::SQSUBv2i32:
  case AArch64::SQSUBv2i64:
  case AArch64::SQSUBv4i32:
  case AArch64::SQSUBv16i8:
  case AArch64::SQSUBv8i16:
    lift_sqsub(opcode);
    break;

  case AArch64::UMULLv4i16_indexed:
  case AArch64::UMULLv2i32_indexed:
  case AArch64::UMULLv8i16_indexed:
  case AArch64::UMULLv4i32_indexed:
  case AArch64::SMULLv4i32_indexed:
  case AArch64::SMULLv8i16_indexed:
  case AArch64::SMULLv4i16_indexed:
  case AArch64::SMULLv2i32_indexed:
    lift_mull(opcode);
    break;

  case AArch64::URHADDv8i8:
  case AArch64::URHADDv16i8:
  case AArch64::URHADDv4i16:
  case AArch64::URHADDv8i16:
  case AArch64::URHADDv2i32:
  case AArch64::URHADDv4i32:
  case AArch64::UHADDv8i8:
  case AArch64::UHADDv16i8:
  case AArch64::UHADDv4i16:
  case AArch64::UHADDv8i16:
  case AArch64::UHADDv2i32:
  case AArch64::UHADDv4i32:
  case AArch64::SRHADDv8i8:
  case AArch64::SRHADDv16i8:
  case AArch64::SRHADDv4i16:
  case AArch64::SRHADDv8i16:
  case AArch64::SRHADDv2i32:
  case AArch64::SRHADDv4i32:
  case AArch64::SHADDv8i8:
  case AArch64::SHADDv16i8:
  case AArch64::SHADDv4i16:
  case AArch64::SHADDv8i16:
  case AArch64::SHADDv2i32:
  case AArch64::SHADDv4i32:
  case AArch64::UHSUBv8i8:
  case AArch64::UHSUBv16i8:
  case AArch64::UHSUBv4i16:
  case AArch64::UHSUBv8i16:
  case AArch64::UHSUBv2i32:
  case AArch64::UHSUBv4i32:
  case AArch64::SHSUBv8i8:
  case AArch64::SHSUBv16i8:
  case AArch64::SHSUBv4i16:
  case AArch64::SHSUBv8i16:
  case AArch64::SHSUBv2i32:
  case AArch64::SHSUBv4i32:
    lift_more_vec_binops(opcode);
    break;

  case AArch64::XTNv2i32:
  case AArch64::XTNv4i32:
  case AArch64::XTNv4i16:
  case AArch64::XTNv8i16:
  case AArch64::XTNv8i8:
  case AArch64::XTNv16i8:
    lift_xtn(opcode);
    break;

    //    case AArch64::UQXTNv1i8:
    //    case AArch64::UQXTNv1i16:
    //    case AArch64::UQXTNv1i32:
  case AArch64::UQXTNv8i8:
  case AArch64::UQXTNv4i16:
  case AArch64::UQXTNv2i32:
  case AArch64::UQXTNv16i8:
  case AArch64::UQXTNv8i16:
  case AArch64::UQXTNv4i32:
    //    case AArch64::SQXTNv1i8:
    //    case AArch64::SQXTNv1i16:
    //    case AArch64::SQXTNv1i32:
  case AArch64::SQXTNv8i8:
  case AArch64::SQXTNv4i16:
  case AArch64::SQXTNv2i32:
  case AArch64::SQXTNv16i8:
  case AArch64::SQXTNv8i16:
  case AArch64::SQXTNv4i32:
    lift_qxtn(opcode);
    break;

    // unary vector instructions
  case AArch64::ABSv1i64:
  case AArch64::ABSv8i8:
  case AArch64::ABSv4i16:
  case AArch64::ABSv2i32:
  case AArch64::ABSv2i64:
  case AArch64::ABSv16i8:
  case AArch64::ABSv8i16:
  case AArch64::ABSv4i32:
  case AArch64::CLZv2i32:
  case AArch64::CLZv4i16:
  case AArch64::CLZv8i8:
  case AArch64::CLZv16i8:
  case AArch64::CLZv8i16:
  case AArch64::CLZv4i32:
  case AArch64::UADALPv8i8_v4i16:
  case AArch64::UADALPv4i16_v2i32:
  case AArch64::UADALPv2i32_v1i64:
  case AArch64::UADALPv4i32_v2i64:
  case AArch64::UADALPv16i8_v8i16:
  case AArch64::UADALPv8i16_v4i32:
  case AArch64::SADALPv8i8_v4i16:
  case AArch64::SADALPv4i16_v2i32:
  case AArch64::SADALPv2i32_v1i64:
  case AArch64::SADALPv4i32_v2i64:
  case AArch64::SADALPv16i8_v8i16:
  case AArch64::SADALPv8i16_v4i32:
  case AArch64::UADDLPv4i16_v2i32:
  case AArch64::UADDLPv2i32_v1i64:
  case AArch64::UADDLPv8i8_v4i16:
  case AArch64::UADDLPv8i16_v4i32:
  case AArch64::UADDLPv4i32_v2i64:
  case AArch64::UADDLPv16i8_v8i16:
  case AArch64::SADDLPv8i8_v4i16:
  case AArch64::SADDLPv4i16_v2i32:
  case AArch64::SADDLPv2i32_v1i64:
  case AArch64::SADDLPv16i8_v8i16:
  case AArch64::SADDLPv8i16_v4i32:
  case AArch64::SADDLPv4i32_v2i64:
  case AArch64::UADDLVv8i16v:
  case AArch64::UADDLVv4i32v:
  case AArch64::UADDLVv8i8v:
  case AArch64::UADDLVv4i16v:
  case AArch64::UADDLVv16i8v:
  case AArch64::SADDLVv8i8v:
  case AArch64::SADDLVv16i8v:
  case AArch64::SADDLVv4i16v:
  case AArch64::SADDLVv8i16v:
  case AArch64::SADDLVv4i32v:
  case AArch64::NEGv1i64:
  case AArch64::NEGv4i16:
  case AArch64::NEGv8i8:
  case AArch64::NEGv16i8:
  case AArch64::NEGv2i32:
  case AArch64::NEGv8i16:
  case AArch64::NEGv2i64:
  case AArch64::NEGv4i32:
  case AArch64::NOTv8i8:
  case AArch64::NOTv16i8:
  case AArch64::CNTv8i8:
  case AArch64::CNTv16i8:
  case AArch64::ADDVv16i8v:
  case AArch64::ADDVv8i16v:
  case AArch64::ADDVv4i32v:
  case AArch64::ADDVv8i8v:
  case AArch64::ADDVv4i16v:
  case AArch64::RBITv8i8:
  case AArch64::RBITv16i8:
    lift_unary_vec(opcode);
    break;

  default:
    visitError();
  }
}
