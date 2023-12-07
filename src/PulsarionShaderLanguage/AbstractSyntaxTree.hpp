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
        TokenNode,
        ScopeNode, // { ... }
        StatementNode, // This is used to seperate statements and has no actual use in AST generation
        ExpressionNode, // This is used to seperate expressions and has no actual use in AST generation, except for order of operations

        IdentifierNode, // a, a::b, a.b
        LiteralNode, // 1, 1.0, 'a'

        IfNode, // if (...) { ... }
        ElseNode, // else { ... }
        ForNode, // for (...) { ... }
        WhileNode, // while (...) { ... }
        StructNode, // struct { ... }
        DoNode, // do { ... } while (...);

        DefaultKeywordNode, // default
        BreakKeywordNode, // break
        ContinueKeywordNode, // continue
        ReturnKeywordNode, // return
        SwitchKeywordNode, // switch (...)
        CaseKeywordNode, // case x:
        UsingKeywordNode, // using a::b;
        NamespaceKeywordNode, // namespace a { ... }

        AssignmentNode, // a = b
        BinaryOperatorNode, // a + b
    };

    PULSARION_SHADER_LANGUAGE_API std::string NodeTypeToString(NodeType type);

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

    protected:
        NodeDescriptor m_descriptor;
        std::vector<SyntaxNode> m_children;
    };

    inline SyntaxNode AssigmentNode(NodeDescriptor descriptor, SyntaxNode left, SyntaxNode right) {
		return SyntaxNode(descriptor, { left, right });
	}

    inline SyntaxNode BinaryOperatorNode(NodeDescriptor descriptor, SyntaxNode left, SyntaxNode right) {
		return SyntaxNode(descriptor, { left, right });
    }

    inline std::string SyntaxNodeToString(SyntaxNode& node, int depth = 0) {
        std::string indent(depth, '-'); // Create an indentation string
        const auto& descriptor = node.GetDescriptor();

        std::stringstream ss;
        ss << indent << "Node Type: " << NodeTypeToString(descriptor.Type) << "\n";
        if (descriptor.Content.has_value()) {
            ss << indent << "Content: " << descriptor.Content.value().Value << " (" << descriptor.Content->Line << ":" << descriptor.Content->Column << ")" << "\n";
        }
        ss << indent << "Start: " << descriptor.Start << ", End: " << descriptor.End << "\n";

        for (auto& child : node.GetChildren()) {
            ss << SyntaxNodeToString(child, depth + 1);
        }
        return ss.str();
    }
}
