#pragma once

#include "Core.hpp"
#include "AbstractSyntaxTree.hpp"
#include "ParserUtil.hpp"

namespace Pulsarion::Shader
{
    class PULSARION_SHADER_LANGUAGE_API TypeChecker
    {
    public:
        explicit TypeChecker(SyntaxNode&& rootAST) : m_AST(std::move(rootAST)) {}

        void GenerateTypeInformation();

        [[nodiscard]] const Parsing::TypeInfo& GetTypeInfo() const { return m_TypeInfo; }

    private:
        SyntaxNode m_AST;
        Parsing::TypeInfo m_TypeInfo;
    };
}
