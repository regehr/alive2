#pragma once

#include <memory>
#include <string>
#include <string_view>
#include <hiredis/hiredis.h>

namespace util {

class Errors;

class Cache {
  redisContext *ctx = nullptr;
public:
  Cache(unsigned port);
  ~Cache();
  bool lookup(const std::string_view s, util::Errors &errs);
  void update(const std::string_view s, const util::Errors &errs);
};

} // namespace
