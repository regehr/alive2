cmake_minimum_required(VERSION 3.20)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ANTLR4_JAR_LOCATION /usr/share/java/antlr-complete.jar CACHE STRING "antlr4 jar location")
find_package(antlr4-generator REQUIRED)
find_package(antlr4-runtime REQUIRED antlr4_shared)

message(STATUS "Found antlr4cpp libs: ${ANTLR4_LIB_DIR} and includes: ${ANTLR4_INCLUDE_DIR} ")

antlr4_generate(Semantics ${CMAKE_CURRENT_SOURCE_DIR}/Semantics.g4 BOTH TRUE TRUE aslt
  "${ANTLR4_TOKEN_FILES_Semantics}"
  "${ANTLR4_TOKEN_DIRECTORY_Semantics}")
message(STATUS "antlr generated include: ${ANTLR4_INCLUDE_DIR_Semantics}")


find_package(LLVM REQUIRED CONFIG) # llvm at head
message(STATUS "Found LLVM: " ${LLVM_DIR} ", tools: " ${LLVM_TOOLS_BINARY_DIR})
# list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
# include(AddLLVM)


add_library(semantics STATIC "${ANTLR4_SRC_FILES_Semantics}")
target_compile_options(semantics PUBLIC -Wno-error=overloaded-virtual -Wno-overloaded-virtual) # warnings in antlr-generated code
target_include_directories(semantics PUBLIC ${PROJECT_BINARY_DIR} ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_Semantics})

add_library(bridge STATIC "aslp_bridge.cpp" "aslt_visitor.cpp")
add_dependencies(bridge antlr4_shared)
target_link_libraries(bridge PUBLIC antlr4_shared semantics)
target_include_directories(bridge PRIVATE "${CMAKE_CURRENT_SOURCE_DIR}/..")

target_compile_definitions(bridge PRIVATE ASLT_DIR="${CMAKE_CURRENT_SOURCE_DIR}/aslt")
target_compile_options(bridge PRIVATE -g -O0 -fsanitize=address -Wno-error=unused-variable)
target_link_options(bridge PRIVATE -g -O0 -fsanitize=address)

execute_process(COMMAND ${LLVM_TOOLS_BINARY_DIR}/llvm-config --libs support core irreader bitwriter
                OUTPUT_VARIABLE llvm_libs OUTPUT_STRIP_TRAILING_WHITESPACE)
message(STATUS "llvm_libs: ${llvm_libs}")
separate_arguments(llvm_libs UNIX_COMMAND "${llvm_libs}")
# target_link_libraries(bridge PRIVATE "${lib_files}")
target_link_options(bridge PUBLIC "${llvm_libs}")
target_link_directories(bridge PUBLIC ${LLVM_LIBRARY_DIR})
target_include_directories(bridge PUBLIC ${LLVM_INCLUDE_DIRS})

add_executable(bridge-bin aslp_main.cpp)
target_link_libraries(bridge-bin PRIVATE bridge)

install(TARGETS bridge-bin DESTINATION bin)

set_target_properties(
  bridge-bin
  PROPERTIES
  OUTPUT_NAME bridge
  RUNTIME_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/bin
  LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)
