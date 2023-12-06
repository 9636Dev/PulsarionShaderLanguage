#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{
    std::string NodeTypeToString(NodeType type)
    {
        switch (type)
        {
        case NodeType::TokenNode: return "TokenNode";
        case NodeType::ScopeNode: return "ScopeNode";
        case NodeType::StatementNode: return "StatementNode";
        case NodeType::LiteralNode: return "LiteralNode";
        case NodeType::IdentifierNode: return "IdentifierNode";
        case NodeType::BinaryOperatorNode: return "BinaryOperatorNode";
        case NodeType::AssignmentNode: return "AssignmentNode";
        }
    }

    SyntaxNode::SyntaxNode(NodeDescriptor descriptor, std::vector<SyntaxNode> children)
        : m_descriptor(descriptor)
        , m_children(children)
    {
    }

    const NodeDescriptor& SyntaxNode::GetDescriptor() const
    {
        return m_descriptor;
    }

    std::vector<SyntaxNode>& SyntaxNode::GetChildren()
    {
        return m_children;
    }
}
