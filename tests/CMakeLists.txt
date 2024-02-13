cmake_minimum_required(VERSION 3.27)
project(PulsarionShaderLanguageTests)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(PULSARION_SHADER_LANGUAGE_TEST_SOURCES
        LexerTest.cpp
        ParserTest.cpp
)
add_executable(PulsarionShaderLanguageTests ${PULSARION_SHADER_LANGUAGE_TEST_SOURCES})

target_link_libraries(PulsarionShaderLanguageTests PRIVATE PulsarionCore)
target_link_libraries(PulsarionShaderLanguageTests PRIVATE PulsarionShaderLanguage)

include(FetchContent)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG main
)
FetchContent_MakeAvailable(googletest)

enable_testing()
include(GoogleTest)

target_link_libraries(PulsarionShaderLanguageTests PRIVATE gtest_main)

gtest_discover_tests(PulsarionShaderLanguageTests)
