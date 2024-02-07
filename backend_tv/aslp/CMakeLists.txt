CMAKE_MINIMUM_REQUIRED(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ANTLR4_JAR_LOCATION /usr/share/java/antlr-complete.jar CACHE STRING "antlr4 jar location")
find_package(antlr4-generator REQUIRED)
find_package(antlr4-runtime REQUIRED antlr4_shared)

message(STATUS "Found antlr4cpp libs: ${ANTLR4_LIB_DIR} and includes: ${ANTLR4_INCLUDE_DIR} ")
message(STATUS "Found antlr: ${ANTLR4_DIR}")


antlr4_generate(Semantics ${CMAKE_CURRENT_SOURCE_DIR}/Semantics.g4 BOTH TRUE TRUE aslt
  "${ANTLR4_TOKEN_FILES_Semantics}"
  "${ANTLR4_TOKEN_DIRECTORY_Semantics}")
message(STATUS "antlr generated include: ${ANTLR4_INCLUDE_DIR_Semantics} ${ANTLR4_SRC_FILES_Semantics}")

include_directories(${PROJECT_BINARY_DIR}  ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_Semantics})

add_executable(bridge aslp_bridge.cpp aslt_visitor.cpp  "${ANTLR4_SRC_FILES_Semantics}" )
add_dependencies(bridge antlr4_shared )
target_link_libraries(bridge antlr4_shared)

install(TARGETS bridge DESTINATION bin)

set_target_properties(
  bridge 
  PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY bin
  LIBRARY_OUTPUT_DIRECTORY lib
  ARCHIVE_OUTPUT_DIRECTORY lib
)
