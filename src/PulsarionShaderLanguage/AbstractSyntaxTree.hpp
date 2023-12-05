#pragma once

#include "Core.hpp"
#include "Token.hpp"

#include <vector>
#include <optional>

namespace Pulsarion::Shader
{
    enum class NodeType
    {
        TokenNode,
        ScopeNode,
    };

    struct NodeDescriptor
    {
        NodeType Type;
        std::optional<Token> Content; 
        std::size_t Start;
        std::size_t End;

        NodeDescriptor(NodeType type, std::size_t start, std::size_t end, std::optional<Token> content = std::nullopt) : Type(type), Start(start), End(end), Content(content) {};
    };

    class PULSARION_SHADER_LANGUAGE_API SyntaxNode
    {
    public:
        SyntaxNode(NodeDescriptor descriptor, std::vector<SyntaxNode> children);
        ~SyntaxNode() = default;

        const NodeDescriptor& GetDescriptor() const;
        std::vector<SyntaxNode>& GetChildren();
        
    private:
        NodeDescriptor m_descriptor;
        std::vector<SyntaxNode> m_children;
    };
}
