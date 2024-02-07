#include <cassert>
#include <iostream>
#include <filesystem>

#include <antlr4-runtime.h>
#include <llvm/ADT/ArrayRef.h>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "SemanticsParser.h"
#include "SemanticsLexer.h"

#include "SemanticsVisitor.h"
#include "aslt_visitor.hpp"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/GlobalValue.h"

namespace fs = std::filesystem;

static llvm::LLVMContext Context;


void parse(const fs::path& path) {
  std::ifstream file{path};

  antlr4::ANTLRInputStream input{file};
  aslt::SemanticsLexer lexer{&input};
  antlr4::CommonTokenStream tokens{&lexer};
  aslt::SemanticsParser parser{&tokens};

  parser.setBuildParseTree(true);

  std::unique_ptr<llvm::Module> Mod = std::make_unique<llvm::Module>("", Context);
  assert(Mod);
  auto funty = llvm::FunctionType::get(llvm::Type::getVoidTy(Context), {}, false);
  auto fun = llvm::Function::Create(funty, llvm::GlobalValue::LinkageTypes::ExternalLinkage, "aslp_entry", Mod.get());

  aslt_visitor visitor{*fun};

  visitor.visitStmts(parser.stmts());
}

int main() {
  std::cout << "Hello World" << std::endl;

  // relative to backend_tv/aslp directory
  fs::path aslt_path{fs::absolute({"./aslt/adds.aslt"})};

  assert(fs::exists(aslt_path) && "aslt does not exist");
  std::cout << "aslt: " << aslt_path << '\n';

  parse(aslt_path);

  return 0;
}
