cmake_minimum_required(VERSION 3.20)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ANTLR4_JAR_LOCATION /usr/share/java/antlr-complete.jar CACHE STRING "antlr4 jar location")
find_package(antlr4-generator REQUIRED)
find_package(antlr4-runtime REQUIRED antlr4_shared)

message(STATUS "Found antlr4cpp libs: ${ANTLR4_LIB_DIR} and includes: ${ANTLR4_INCLUDE_DIR} ")

antlr4_generate(Semantics ${CMAKE_CURRENT_SOURCE_DIR}/Semantics.g4 BOTH TRUE TRUE aslt
  "${ANTLR4_TOKEN_FILES_Semantics}"
  "${ANTLR4_TOKEN_DIRECTORY_Semantics}")
message(STATUS "antlr generated include: ${ANTLR4_INCLUDE_DIR_Semantics}")

include_directories(${PROJECT_BINARY_DIR} ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_Semantics})

find_package(LLVM REQUIRED) # llvm at head
message(STATUS "Found LLVM: " ${LLVM_DIR})

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})


add_executable(bridge "aslp_bridge.cpp" "aslt_visitor.cpp" "${ANTLR4_SRC_FILES_Semantics}")
add_dependencies(bridge antlr4_shared )
target_link_libraries(bridge PUBLIC antlr4_shared)


llvm_map_components_to_libnames(llvm_libs support core irreader bitwriter)
message(STATUS "llvm_libs: ${llvm_libs}")
target_link_libraries(bridge PRIVATE ${llvm_libs})

install(TARGETS bridge DESTINATION bin)

set_target_properties(
  bridge 
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY bin
  LIBRARY_OUTPUT_DIRECTORY lib
  ARCHIVE_OUTPUT_DIRECTORY lib
)
