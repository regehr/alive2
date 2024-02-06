CMAKE_MINIMUM_REQUIRED(VERSION 3.10)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(ANTLR4_JAR_LOCATION /usr/share/java/antlr-complete.jar CACHE STRING "antlr4 jar location")
find_package(antlr4-generator REQUIRED)
find_package(antlr4-runtime REQUIRED antlr4_shared)

message(STATUS "Found antlr4cpp libs: ${ANTLR4_LIB_DIR} and includes: ${ANTLR4_INCLUDE_DIR} ")
message(STATUS "Found antlr: ${ANTLR4_DIR}")


antlr4_generate(Semantics ${CMAKE_CURRENT_SOURCE_DIR}/Semantics.g4 BOTH TRUE TRUE grammar
  "${ANTLR4_TOKEN_FILES_Semantics}"
  "${ANTLR4_TOKEN_DIRECTORY_Semantics}")
message(STATUS "antlr generated include: ${ANTLR4_INCLUDE_DIR_Semantics} ${ANTLR4_SRC_FILES_Semantics}")

include_directories( ${PROJECT_BINARY_DIR}  ${ANTLR4_INCLUDE_DIR} ${ANTLR4_INCLUDE_DIR_Semantics})

add_executable(grammar main.cpp  "${ANTLR4_SRC_FILES_Semantics}" )
add_dependencies(grammar antlr4_shared )
target_link_libraries(grammar antlr4_shared)

install(TARGETS grammar DESTINATION bin)

set_target_properties(grammar PROPERTIES
  RUNTIME_OUTPUT_DIRECTORY bin)
