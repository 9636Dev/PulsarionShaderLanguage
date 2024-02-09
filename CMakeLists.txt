cmake_minimum_required(VERSION 3.27)
project(PulsarionShaderLanguage VERSION 0.0.1 LANGUAGES CXX DESCRIPTION "Pulsarion Shader Language Library")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(PULSARION_SHADER_LANGUAGE_SOURCES
    src/PulsarionShaderLanguage/Core.hpp
    src/PulsarionShaderLanguage/Core.cpp
    src/PulsarionShaderLanguage/PulsarionShaderLanguage.hpp
    src/PulsarionShaderLanguage/PulsarionShaderLanguage.cpp
    src/PulsarionShaderLanguage/Token.hpp
    src/PulsarionShaderLanguage/Token.cpp
    src/PulsarionShaderLanguage/Lexer.hpp
    src/PulsarionShaderLanguage/Lexer.cpp
    src/PulsarionShaderLanguage/Parser.hpp
    src/PulsarionShaderLanguage/Parser.cpp
    src/PulsarionShaderLanguage/ParserUtil.hpp
    src/PulsarionShaderLanguage/ParserUtil.cpp
    src/PulsarionShaderLanguage/AbstractSyntaxTree.hpp
    src/PulsarionShaderLanguage/AbstractSyntaxTree.cpp
    src/PulsarionShaderLanguage/CodeGeneration.hpp
    src/PulsarionShaderLanguage/CodeGeneration.cpp
    src/PulsarionShaderLanguage/Optimizer.hpp
    src/PulsarionShaderLanguage/Optimizer.cpp
)

if (NOT DEFINED PULSARION_LIBRARY_TYPE)
    message(FATAL_ERROR "PULSARION_LIBRARY_TYPE is not defined, please include PulsarionCore first!")
    return()
endif()

add_library(PulsarionShaderLanguage ${PULSARION_LIBRARY_TYPE} ${PULSARION_SHADER_LANGUAGE_SOURCES})

if (PULSARION_LIBRARY_TYPE STREQUAL "SHARED")
    target_compile_definitions(PulsarionShaderLanguage PUBLIC PULSARION_SHADER_LANGUAGE_BUILD_DLL)
endif()

target_include_directories(PulsarionShaderLanguage PUBLIC src)

add_dependencies(PulsarionShaderLanguage Pulsarion::Core)
target_link_libraries(PulsarionShaderLanguage PUBLIC Pulsarion::Core)

add_subdirectory(tests)
