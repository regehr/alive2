#!/usr/bin/env python3

import sys

def main():
  assert len(sys.argv) >= 2, "requires AArch64GenInstrInfo.inc file as first argument"
  with open(sys.argv[1], 'r') as f:
    inc = f.readlines()

  # redirect output to second command line argument 
  if len(sys.argv) >= 3:
    sys.stdout = open(sys.argv[2], 'w')

  split = inc.index('#endif // GET_INSTRINFO_ENUM\n')
  assert split >= 0
  instructions = [(s[0].strip(), s[1].strip().strip(',')) for s in map(lambda x: x.split('='), inc[:split]) if len(s) == 2]

  # generate reverse mapping
  print('#pragma once\n')
  print('#include <map>')
  print('#include <string>\n')

  print('#ifndef AARCH64_MAP_IMPL')
  print('')
  print('namespace aslp {')
  print('  const std::map<std::string, unsigned int>& aarch64_map();')
  print('  const std::map<unsigned int, const std::string>& aarch64_revmap();')
  print('}')
  print('')
  print('#else')
  print()
  print('namespace aslp {')
  print('const std::map<std::string, unsigned int>& aarch64_map() {')
  print('  static std::map<std::string, unsigned int> map;')
  print('  if (map.size() == 0) {')
  for name, code in instructions:
    print(f'    map.emplace("{name}", {code}u);')
  print('  }')
  print('  return map;')
  print('}')

  print('const std::map<unsigned int, const std::string>& aarch64_revmap() {')
  print('  static std::map<unsigned int, const std::string> map;')
  print('  if (map.size() == 0) {')
  for name, code in instructions:
    print(f'    map.emplace({code}u, "{name}");')
  print('  }')
  print('  return map;')
  print('}')

  print('\n} // namespace aslp\n')

  for name, code in instructions:
    print(f'static_assert(llvm::AArch64::{name} == {code}u);')
  print('#endif')


if __name__ == '__main__':
  main()

