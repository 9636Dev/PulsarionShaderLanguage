cmake_minimum_required(VERSION 3.27)
project(PulsarionShaderLanguageTests)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(PULSARION_SHADER_LANGUAGE_TEST_SOURCES
    PreprocessorTest.cpp
    LexerTest.cpp
    ParserTest.cpp
)
add_executable(PulsarionShaderLanguageTests ${PULSARION_SHADER_LANGUAGE_TEST_SOURCES})

target_link_libraries(PulsarionShaderLanguageTests PRIVATE PulsarionCore)
target_link_libraries(PulsarionShaderLanguageTests PRIVATE PulsarionShaderLanguage)

if (MSVC)
    set_target_properties(PulsarionShaderLanguageTests PROPERTIES
      VS_DEBUGGER_WORKING_DIRECTORY "${CMAKE_SOURCE_DIR}/build"
    )
endif()

if (NOT TARGET gtest_main)
    add_subdirectory(vendor/googletest)
endif()

enable_testing()
include(GoogleTest)


target_link_libraries(PulsarionShaderLanguageTests PRIVATE gtest_main)

gtest_discover_tests(PulsarionShaderLanguageTests)
