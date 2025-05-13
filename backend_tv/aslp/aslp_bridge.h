#pragma once

#include <filesystem>
#include <functional>
#include <variant>

#include "llvm/MC/MCCodeEmitter.h"
#include "llvm/MC/MCInst.h"
#include "llvm/MC/MCInstrAnalysis.h"
#include "llvm/MC/MCSubtargetInfo.h"

#include "interface.h"
#include "aslt_visitor.h"

#include <aslp-cpp/aslp-cpp.hpp>


namespace aslp {

// an opcode for ASLP is four bytes (= 32 bits), in Arm's conventional little-endian order.
using opcode_t = std::array<uint8_t, 4>;

// encoding name + semantics
using result_t = std::tuple<std::string, stmt_t>;

enum struct err_t {
  missing,
  banned, 
};

struct config_t {
  bool enable;
  bool debug;
  bool fail_if_missing;
  bool vectors;
  std::vector<unsigned> mcinst_banned;

  std::string server_addr;
  unsigned server_port;
};

class bridge {
  lifter_interface_llvm& iface;
  llvm::LLVMContext& context;
  const llvm::MCCodeEmitter& mce;
  const llvm::MCSubtargetInfo& sti;
  const llvm::MCInstrAnalysis& ia;
  aslp_connection conn;

public:
  bridge(lifter_interface_llvm&, const llvm::MCCodeEmitter&, const llvm::MCSubtargetInfo&, const llvm::MCInstrAnalysis&);

  std::variant<err_t, result_t> run(const llvm::MCInst& inst, const opcode_t& bytes);
  static const config_t& config();

protected:
  using parsed_t = std::reference_wrapper<aslt::SemanticsParser::StmtsContext>;

  std::variant<err_t, result_t> run_special(const llvm::MCInst& inst, const opcode_t& bytes);
  std::variant<err_t, stmt_t> parse(std::string_view aslt);
};

std::string format_opcode(const opcode_t& bytes);
uint32_t get_opnum(const opcode_t &bytes);

static_assert(!std::is_abstract<aslt_visitor>(), "aslt_visitor must not be abstract");

};
