#pragma once

#include "Core.hpp"
#include "AbstractSyntaxTree.hpp"

#include <memory>
#include <string>
#include <utility>

namespace Pulsarion::Shader
{

    struct GenerateOptions
    {
        bool GenerateMinified;

        GenerateOptions()
            : GenerateMinified(false)
        {
        }
    };

    struct GeneratorMetadata
    {
        std::string Name;
        std::string Version;
        std::string Description;
    };

    template<typename T>
    concept MetaData = std::is_base_of<GeneratorMetadata, T>::value;

    class BaseGenerator
    {
    public:
        using SourceCode = std::string;

        virtual ~BaseGenerator() = default;

        virtual std::pair<SourceCode, std::unique_ptr<GeneratorMetadata>> Generate(SyntaxNode& ast) = 0;
    };

    struct CppGeneratorMetadata : public GeneratorMetadata
    {
        std::string Standard;

        CppGeneratorMetadata()
        {
            Name = "C++";
            Version = "1.0.0";
            Description = "Generates C++ code";
            Standard = "C++17";
        }
    };


    class CppGenerator : public BaseGenerator
    {
    public:
        CppGenerator() = default;
        ~CppGenerator() override = default;

        std::pair<SourceCode, std::unique_ptr<GeneratorMetadata>> Generate(SyntaxNode& ast) override;
    };
}
