cmake_minimum_required(VERSION 3.27)
project(PulsarionShaderLanguage VERSION 0.0.1 LANGUAGES CXX DESCRIPTION "Pulsarion Shader Language Library")
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(PULSARION_SHADER_LANGUAGE_SOURCES
    src/PulsarionShaderLanguage/PulsarionShaderLanguage.hpp
    src/PulsarionShaderLanguage/PulsarionShaderLanguage.cpp
    src/PulsarionShaderLanguage/Lexer.hpp
    src/PulsarionShaderLanguage/Lexer.cpp
    src/PulsarionShaderLanguage/ContextFreeGrammar.hpp
    src/PulsarionShaderLanguage/ContextFreeGrammar.cpp
    src/PulsarionShaderLanguage/AbstractSyntaxTree.hpp
    src/PulsarionShaderLanguage/AbstractSyntaxTree.cpp
)

if (NOT DEFINED PULSARION_LIBRARY_TYPE)
    message(FATAL_ERROR "PULSARION_LIBRARY_TYPE is not defined, please include PulsarionCore first!")
    return()
endif()

add_library(PulsarionShaderLanguage ${PULSARION_LIBRARY_TYPE} ${PULSARION_SHADER_LANGUAGE_SOURCES})

target_include_directories(PulsarionShaderLanguage PUBLIC src)

target_link_libraries(PulsarionShaderLanguage PUBLIC Pulsarion::Core)
