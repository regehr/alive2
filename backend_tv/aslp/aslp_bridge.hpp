#pragma once

#include <filesystem>
#include <functional>

#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCSubtargetInfo.h"

#include "interface.hpp"
#include "aslt_visitor.hpp"


namespace aslp {

// an opcode for ASLP is four bytes (= 32 bits), in Arm's conventional little-endian order.
using opcode_t = std::array<uint8_t, 4>;

enum struct err_t {
  missing,
  banned, 
};

class bridge {
  lifter_interface& iface;
  llvm::LLVMContext& context;
  const llvm::MCCodeEmitter& mce;
  const llvm::MCSubtargetInfo& sti;
  const llvm::MCInstrAnalysis& ia;

  std::unique_ptr<aslt::SemanticsParser> parser; // XXX not thread-safe

public:
  bridge(lifter_interface&, const llvm::MCCodeEmitter&, const llvm::MCSubtargetInfo&, const llvm::MCInstrAnalysis&);

  std::variant<err_t, stmt_t> run(const llvm::MCInst& inst, const opcode_t& bytes);


protected:
  using parsed_t = std::reference_wrapper<aslt::SemanticsParser::StmtsContext>;

  std::variant<err_t, stmt_t> parse(const std::filesystem::path&);
};

std::string format_opcode(const opcode_t& bytes);
uint32_t get_opnum(const opcode_t &bytes);

static_assert(!std::is_abstract<aslt_visitor>(), "aslt_visitor must not be abstract");

};
