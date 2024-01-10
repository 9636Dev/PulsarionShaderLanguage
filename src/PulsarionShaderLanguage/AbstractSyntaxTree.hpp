#pragma once

#include "Core.hpp"
#include "Token.hpp"

#include <vector>
#include <optional>
#include <sstream>

namespace Pulsarion::Shader
{
    enum class NodeType
    {
        Scope,
    };

    struct PULSARION_SHADER_LANGUAGE_API SyntaxNode
    {
    public:

        NodeType Type;
        std::optional<Token> Content;
        std::vector<SyntaxNode> Children;
    };

}
