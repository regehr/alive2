cmake_minimum_required(VERSION 3.20)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ANTLR4_JAR_LOCATION /usr/share/java/antlr-complete.jar CACHE STRING "antlr4 jar location")
find_package(antlr4-generator REQUIRED)
find_package(antlr4-runtime REQUIRED antlr4_shared)

message(STATUS "Found antlr4cpp libs: ${ANTLR4_LIB_DIR} and includes: ${ANTLR4_INCLUDE_DIR} ")

antlr4_generate(Grammar ${CMAKE_CURRENT_SOURCE_DIR}/Semantics.g4 BOTH TRUE TRUE aslt
  "${ANTLR4_TOKEN_FILES_Grammar}"
  "${ANTLR4_TOKEN_DIRECTORY_Grammar}")
message(STATUS "antlr generated include: ${ANTLR4_INCLUDE_DIR_Grammar}")

if (NOT LLVM_FOUND)
  # find_package(LLVM REQUIRED CONFIG)
  message(FATAL_ERROR "LLVM not found. the aslp CMakeList should be imported by the root CMakeList with LLVM already located")
else()
  message(STATUS "... LLVM already found: ${LLVM_DIR}")
endif()

find_file(AARCH64_INSTR_INC "Target/AArch64/AArch64GenInstrInfo.inc" REQUIRED PATHS ${LLVM_BINARY_DIR}/lib ${LLVM_INCLUDE_DIRS})
message(STATUS "AArch64GenInstrInfo.inc: ${AARCH64_INSTR_INC}")

include(FetchContent)
FetchContent_Declare(aslp-cpp
  URL https://github.com/UQ-PAC/aslp/archive/34cfa404635becab3dcb15d60e1bf0d4635f3e93.zip
  # GIT_REPOSITORY https://github.com/UQ-PAC/aslp.git
  # GIT_TAG        clientserver
  SOURCE_SUBDIR  aslp-cpp
  DOWNLOAD_EXTRACT_TIMESTAMP false
  FIND_PACKAGE_ARGS
)
FetchContent_MakeAvailable(aslp-cpp)
FetchContent_GetProperties(aslp-cpp SOURCE_DIR ASLP_DIR)
message(STATUS "Found aslp-cpp: ${ASLP_DIR} ${aslp-cpp_DIR}")

add_library(grammar STATIC "${ANTLR4_SRC_FILES_Grammar}")
target_compile_options(grammar PUBLIC -Wno-error=overloaded-virtual -Wno-overloaded-virtual) # warnings in antlr-generated code
target_include_directories(grammar PUBLIC ${PROJECT_BINARY_DIR} ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_Grammar})

add_llvm_library(bridge "aslp_bridge.cpp" "aslt_visitor.cpp")
add_dependencies(bridge antlr4_shared)
target_link_libraries(bridge PUBLIC aslp-cpp::aslp-cpp)
target_link_libraries(bridge PUBLIC antlr4_shared grammar)
target_include_directories(bridge PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")

target_compile_definitions(bridge PRIVATE ASLT_DIR="${CMAKE_CURRENT_SOURCE_DIR}/aslt")

find_package(Python)
add_custom_command(
  OUTPUT aarch64_map.h
  COMMAND
    ${Python_EXECUTABLE}
    ${CMAKE_CURRENT_SOURCE_DIR}/../scripts/parse_tablegen.py
    ${AARCH64_INSTR_INC}
    aarch64_map.h
  DEPENDS ${AARCH64_INSTR_INC}
  VERBATIM
)
add_custom_target(aarch64_map DEPENDS aarch64_map.h)
add_dependencies(bridge aarch64_map)
target_include_directories(bridge PUBLIC "${CMAKE_CURRENT_BINARY_DIR}/..")  # for aarch64_map.h

add_llvm_executable(bridge-bin "tools/aslp_main.cpp")
target_link_libraries(bridge-bin PRIVATE bridge)
target_include_directories(bridge-bin PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}")

install(TARGETS bridge-bin DESTINATION bin)

set_target_properties(
  bridge-bin
  PROPERTIES
  OUTPUT_NAME bridge
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)
