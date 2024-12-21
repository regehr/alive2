#include <cassert>
#include <cstdlib>
#include <iostream>
#include <stdexcept>
#include <filesystem>
#include <memory>
#include <optional>
#include <ranges>

#include <antlr4-runtime.h>
#include <llvm/ADT/ArrayRef.h>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "SemanticsParser.h"
#include "SemanticsLexer.h"

#include <aslp/aarch64_map.h>
#include "interface.h"
#include "aslt_visitor.h"
#include "aslp_bridge.h"
#include <aslp-cpp/aslp-cpp.hpp>

#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/MC/MCExpr.h"
#include "llvm/MC/MCInstrAnalysis.h"

namespace fs = std::filesystem;


bool getenv_bool(const std::string& name, bool def = false) {
  auto env = std::getenv(name.c_str());
  if (!env) return def;

  std::string str{env};
  std::ranges::transform(str, str.begin(), 
      [](char c){ return std::tolower(c); });

  if (str == "1" || str == "on" || str == "yes" || str == "true")
    return true;
  if (str == "0" || str == "off" || str == "no" || str == "false")
    return false;
  throw std::invalid_argument("could not parse boolean value: " + str);
}

aslp_connection make_conn() {
  auto config = aslp::bridge::config();
  static std::multimap<std::string, std::string> extra_params; // XXX sloppy use of static
  if (config.vectors && !extra_params.contains("flags")) {
    extra_params.insert({"flags", "+dis:vectors"});
  }
  auto conn = aslp_connection{config.server_addr, static_cast<int>(config.server_port), extra_params};
  conn.wait_active();
  return conn;
}

namespace aslp {

bridge::bridge(lifter_interface_llvm& iface, const llvm::MCCodeEmitter& mce, const llvm::MCSubtargetInfo& sti, const llvm::MCInstrAnalysis& ia) 
  : iface{iface}, context{iface.ll_function().getContext()}, mce{mce}, sti{sti}, ia{ia}, conn{make_conn()} {
  (void)this->mce;
  (void)this->sti;
}


const config_t& bridge::config() {
  static bool inited{false};
  static config_t config;

  using namespace std::ranges::views;

  if (!inited) {
    const char* var = "(unknown)";
    try {
      // note! inline assignment expressions below
      config.enable = getenv_bool(var = "ASLP", true);
      config.debug = getenv_bool(var = "ASLP_DEBUG", false);
      config.fail_if_missing = getenv_bool(var = "ASLP_FAIL_MISSING", false);
      config.vectors = getenv_bool(var = "ASLP_VECTORS", true);

      var = "ASLP_BANNED";
      const char* ban_env = std::getenv(var);
      ban_env = ban_env ? ban_env : "";

      config.mcinst_banned.clear();
      for (auto ban_str : std::string{ban_env} | split(',')) {
        auto ban = std::stoul(std::string{ban_str.begin(), ban_str.end()});
        config.mcinst_banned.push_back(ban);
      }

      var = "ASLP_SERVER";
      const char* serv_env = std::getenv(var);
      serv_env = serv_env ? serv_env : "localhost";
      std::string s{serv_env};
      auto split = s.find(':');

      config.server_addr = s.substr(0, split);
      auto port_str = split != s.npos ? s.substr(split + 1) : "8000";
      config.server_port = std::stoul(port_str);

      inited = true;

    } catch (const std::invalid_argument& e) {
      std::cerr 
        << "\nERROR while parsing aslp environment variable: "
        << var << '\n' << std::endl;
      throw e;
    }
  }
  return config;
}


std::variant<err_t, stmt_t> bridge::parse(std::string_view aslt) {
  // std::ifstream file{path};
  // if (file.fail()) {
  //   return err_t::missing;
  // }

  antlr4::ANTLRInputStream input{aslt};
  aslt::SemanticsLexer lexer{&input};
  antlr4::CommonTokenStream tokens{&lexer};

  aslt::SemanticsParser parser{&tokens};

  aslt_visitor visitor{iface, config().debug};

  auto ctx = parser.stmt_lines();
  assert(ctx && "parsing failed! syntax error?");

  return std::any_cast<stmt_t>(visitor.visitStmt_lines(ctx));
}

#ifndef ASLT_DIR
#define ASLT_DIR "./aslt"
#endif


std::variant<err_t, result_t> bridge::run_special(const llvm::MCInst& inst, const opcode_t& bytes) {
  llvm::BasicBlock* bb{nullptr};
  std::string name{"special_unknown"};

  auto& opname = aarch64_revmap().at(inst.getOpcode());
  if (opname == "ADR" || opname == "ADRP") { // ADRP
      assert(inst.getOperand(0).isReg());

      name = "special_adrp";

      bb = llvm::BasicBlock::Create(
          context, "aslp_" + iface.nextName() + "_special", &iface.ll_function());
      iface.set_bb(bb);


      auto expr = inst.getOperand(1).getExpr();
      expr_t global = iface.lookupExprVar(*expr);

      std::string sss;
      llvm::raw_string_ostream ss(sss);
      expr->print(ss, nullptr);
      if (sss.length() > 0 && sss.at(0) == ':') {
        // relocation, probably...

        // create an alloc to emulate the behaviour of got. the got location, which is
        // referenced by :got:varname, stores a pointer to the actual variable location.
        // here, a new alloca fakes this indirection. 
        auto alloc = iface.createAlloca(global->getType(), nullptr, iface.nextName());
        iface.createStore(global, alloc);
        global = alloc;
      }

      iface.updateOutputReg(global);
  } 

  if (bb)
    return std::make_tuple(name, stmt_t{bb, bb});
  else
    return err_t::missing;
}


std::variant<err_t, result_t> bridge::run(const llvm::MCInst& inst, const opcode_t& bytes) {
  const auto& mcinst_banned = config().mcinst_banned;
  static const std::vector<unsigned int> mcinst_banned_opcodes{
    aarch64_map().at("PRFMl"),
    aarch64_map().at("PRFMroW"),
    aarch64_map().at("PRFMroX"),
    aarch64_map().at("PRFMui"),
    aarch64_map().at("PRFUMi"),
    aarch64_map().at("PACIASP"),
    aarch64_map().at("PACIBSP"),
    aarch64_map().at("AUTIASP"),
    aarch64_map().at("AUTIBSP"),
    aarch64_map().at("HINT"),
    aarch64_map().at("BRK"),
  };

  bool banned = !config().enable
    || ia.isBranch(inst)
    || ia.isReturn(inst)
    || ia.isCall(inst)
    || ia.isIndirectBranch(inst)
    || std::find(mcinst_banned_opcodes.begin(), mcinst_banned_opcodes.end(), inst.getOpcode()) != mcinst_banned_opcodes.end()
    || std::ranges::count(mcinst_banned, inst.getOpcode()) != 0
    ;
  if (banned)
    return err_t::banned;

  auto special = run_special(inst, bytes);
  if (special != std::variant<err_t, result_t>{err_t::missing}) {
    return special;
  }

  std::string encoding;
  std::string semantics;
  try {
    std::tie(encoding, semantics) = conn.get_opcode(get_opnum(bytes));
  } catch (const std::runtime_error& e) {
    std::cerr << "aslp_client reported error during disassembly: " << e.what() << std::endl;
    return err_t::missing;
  }

  // fs::path aslt_path{ASLT_DIR};
  // aslt_path /= opstr + ".aslt";
  // aslt_path = std::filesystem::absolute(aslt_path);

  auto parsed = parse(semantics);
  auto err = std::get_if<err_t>(&parsed);
  if (err)
    return *err;

  // std::cerr << "ASLP FINISHED!" << std::endl;
  return std::make_tuple(encoding, std::get<stmt_t>(parsed));
}


std::string format_opcode(const opcode_t &bytes) {
  return std::format(
      "{:02x} {:02x} {:02x} {:02x}",
      bytes.at(0), bytes.at(1), bytes.at(2), bytes.at(3));
}

uint32_t get_opnum(const opcode_t &bytes) {
  uint32_t a64opcode = 0;
  for (const auto& x : std::views::reverse(bytes)) {
    a64opcode <<= 8;
    a64opcode |= (0xff & (uint32_t)x);
  }
  return a64opcode;
}

} // namespace aslp

