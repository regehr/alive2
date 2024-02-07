#include <cassert>
#include <iostream>
#include <filesystem>

#include <antlr4-runtime.h>
#include "ANTLRInputStream.h"
#include "CommonTokenStream.h"
#include "SemanticsParser.h"
#include "SemanticsLexer.h"

#include "SemanticsVisitor.h"
#include "aslt_visitor.hpp"

namespace fs = std::filesystem;


void parse(const fs::path& path) {
  std::ifstream file{path};

  antlr4::ANTLRInputStream input{file};
  aslt::SemanticsLexer lexer{&input};
  antlr4::CommonTokenStream tokens{&lexer};
  aslt::SemanticsParser parser{&tokens};

  parser.setBuildParseTree(true);

  aslt_visitor visitor{};
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
