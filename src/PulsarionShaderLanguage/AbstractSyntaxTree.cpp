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
        case NodeType::ExpressionNode: return "ExpressionNode";
        case NodeType::LiteralNode: return "LiteralNode";
        case NodeType::IdentifierNode: return "IdentifierNode";
        case NodeType::BinaryOperatorNode: return "BinaryOperatorNode";
        case NodeType::AssignmentNode: return "AssignmentNode";
        case NodeType::IfNode: return "IfNode";
        case NodeType::ElseNode: return "ElseNode";
        case NodeType::WhileNode: return "WhileNode";
        case NodeType::ForNode: return "ForNode";
        case NodeType::DoNode: return "DoNode";
        case NodeType::CaseKeywordNode: return "CaseKeywordNode";
        case NodeType::DefaultKeywordNode: return "DefaultKeywordNode";
        case NodeType::BreakKeywordNode: return "BreakNode";
        case NodeType::ContinueKeywordNode: return "ContinueNode";
        case NodeType::ReturnKeywordNode: return "ReturnNode";
        case NodeType::SwitchKeywordNode: return "SwitchNode";
        case NodeType::UsingKeywordNode: return "UsingNode";
        case NodeType::NamespaceKeywordNode: return "NamespaceNode";
        case NodeType::StructNode: return "StructNode";
        case NodeType::AnnotationNode: return "AnnotationNode";
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
