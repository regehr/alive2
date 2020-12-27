// Copyright (c) 2018-present The Alive2 Authors.
// Distributed under the MIT license that can be found in the LICENSE file.

#include "util/parallel.h"

bool unrestricted::init(int max_subprocesses) {
  return true;
}

std::tuple<pid_t, std::ostream *, int> unrestricted::limitedFork() {
  static int nextIndex = 0;
  ++nextIndex;
  return {nextIndex, nullptr, nextIndex };
}

void unrestricted::finishChild(bool is_timeout) {
}

void unrestricted::waitForAllChildren() {
}
