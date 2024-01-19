#pragma once

#include "Core.hpp"
#include "Token.hpp"

#include <vector>
#include <optional>

namespace Pulsarion::Shader
{
    enum class NodeType
    {
        Scope,
        Token,
    };

    PULSARION_SHADER_LANGUAGE_API std::string NodeTypeToString(NodeType type);

    struct PULSARION_SHADER_LANGUAGE_API SyntaxNode
    {
        NodeType Type;
        std::optional<Token> Content;
        SourceLocation Location;
        std::vector<SyntaxNode> Children;

        SyntaxNode(NodeType type, SourceLocation location, std::optional<Token> content = std::nullopt, std::vector<SyntaxNode> children = std::vector<SyntaxNode>())
            : Type(type), Content(content), Location(location), Children(children)
        {
        }

        explicit SyntaxNode(Token token)
            : Type(NodeType::Token), Content(token), Location(token.Location), Children()
        {
        }

        std::string ToString() const;
    };

}
