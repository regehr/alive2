#!/usr/bin/env python3

import sys

def main():
  assert len(sys.argv) >= 2, "requires AArch64GenInstrInfo.inc file as first argument"
  with open(sys.argv[1], 'r') as f:
    inc = f.readlines()

  split = inc.index('#endif // GET_INSTRINFO_ENUM\n')
  assert split >= 0
  instructions = [(s[0].strip(), s[1].strip().strip(',')) for s in map(lambda x: x.split('='), inc[:split]) if len(s) == 2]

  # generate reverse mapping
  print('#pragma once\n')
  print('#include <map>')
  print('#include <string>\n')
  # print('#define GET_INSTRINFO_ENUM')
  # print('#include "Target/AArch64/AArch64GenInstrInfo.inc"\n')
  print('namespace aslp {')
  print('const std::map<std::string, int>& aarch64_map() {')
  print('  static std::map<std::string, int> map;')
  print('  if (map.size() == 0) {')
  for name, code in instructions:
    print(f'    map.emplace("{name}", {code});')
  print('  }')
  print('  return map;')
  print('}')

  print('const std::map<int, const std::string>& aarch64_revmap() {')
  print('  static std::map<int, const std::string> map;')
  print('  if (map.size() == 0) {')
  for name, code in instructions:
    print(f'    map.emplace({code}, "{name}");')
  print('  }')
  print('  return map;')
  print('}')

  print('\n} // namespace aslp\n')

  print('#ifdef AARCH64_MAP_CHECK')
  for name, code in instructions:
    print(f'static_assert(llvm::AArch64::{name} == {code});')
  print('#endif')


if __name__ == '__main__':
  main()
