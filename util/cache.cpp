#include "cache.h"
#include "errors.h"

#include <cassert>
#include <iostream>
#include <sstream>
#include <string>

#include <nop/serializer.h>
#include <nop/structure.h>
#include <nop/utility/stream_writer.h>
#include <nop/utility/stream_reader.h>

using namespace std;

namespace util {

Cache::Cache(unsigned port) {
  const char *hostname = "127.0.0.1";
  struct timeval timeout = { 1, 500000 }; // 1.5 seconds
  ctx = redisConnectWithTimeout(hostname, port, timeout);
  if (!ctx) {
    cerr << "Can't allocate redis context\n";
    exit(-1);
  }
  if (ctx->err) {
    cerr << "Redis connection error: " << ctx->errstr << "\n";
    exit(-1);
  }
}

Cache::~Cache() {
  redisFree(ctx);
}

static bool remote_get(string_view Key, string_view Field, string &Value,
                       redisContext *ctx) {
  assert(ctx);
  redisReply *reply = (redisReply *)redisCommand(ctx, "HGET %s %s", Key.data(),
                                                 Field.data());
  if (!reply || ctx->err) {
    cerr << "Redis error: " << ctx->errstr << "\n";
    exit(-1);
  }
  if (reply->type == REDIS_REPLY_NIL) {
    freeReplyObject(reply);
    return false;
  } else if (reply->type == REDIS_REPLY_STRING) {
    Value = reply->str;
    freeReplyObject(reply);
    return true;
  } else {
    cerr << "Redis protocol error for cache lookup, didn't expect reply type " <<
      to_string(reply->type) << "\n";
    exit(-1);
  }
}

static void remote_put(string_view Key, string_view Field, string_view Value,
                       redisContext *ctx) {
  assert(ctx);
  redisReply *reply = (redisReply *)redisCommand(ctx, "HSET %s %s %s",
      Key.data(), Field.data(), Value.data());
  if (!reply || ctx->err) {
    cerr << "Redis error: " << ctx->errstr << "\n";
    exit(-1);
  }
  if (reply->type != REDIS_REPLY_INTEGER) {
    cerr << "Redis protocol error for cache fill, didn't expect reply type " <<
      to_string(reply->type) << "\n";
  }
  freeReplyObject(reply);
}

bool Cache::lookup(const string_view s, Errors &errs) {
  string err_str;
  if (!remote_get(s, "cache", err_str, ctx)) {
    return false;
  }
  nop::Deserializer<nop::StreamReader<std::stringstream>> deserializer{err_str};
  if (!deserializer.Read(&errs)) {
    cerr << "fatal cache deserialization error\n";
    exit(-1);
  }
  return true;
}

void Cache::update(const string_view s, const Errors &errs) {
  nop::Serializer<nop::StreamWriter<stringstream>> serializer;
  serializer.Write(errs);
  const string data = serializer.writer().stream().str();
  remote_put(s, "cache", data, ctx);
}
  
}
