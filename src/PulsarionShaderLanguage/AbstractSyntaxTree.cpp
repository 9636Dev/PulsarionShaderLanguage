#include "AbstractSyntaxTree.hpp"

#include <sstream>

namespace Pulsarion::Shader
{
    std::string NodeTypeToString(NodeType type)
    {
        switch (type)
        {
        case NodeType::Scope:
            return "Scope";
        case NodeType::Statement:
            return "Statement";
        case NodeType::Token:
            return "Token";
        case NodeType::NumericLiteral:
            return "NumericLiteral";
        case NodeType::BooleanLiteral:
            return "BooleanLiteral";
        case NodeType::Identifier:
            return "Identifier";
        case NodeType::Namespace:
            return "Namespace";
        case NodeType::NumericNegation:
            return "NumericNegation";
        case NodeType::NumericPreIncrement:
            return "NumericPreIncrement";
        case NodeType::NumericPreDecrement:
            return "NumericPreDecrement";
        case NodeType::NumericPostIncrement:
            return "NumericPostIncrement";
        case NodeType::NumericPostDecrement:
            return "NumericPostDecrement";
        case NodeType::NumericBitwiseNot:
            return "NumericBitwiseNot";
        case NodeType::BooleanNegation:
            return "BooleanNegation";
        case NodeType::BinaryBooleanOperation:
            return "BinaryBooleanOperation";
        case NodeType::BinaryNumericOperation:
            return "BinaryNumericOperation";
        case NodeType::BinaryComparisonOperation:
            return "BinaryComparisonOperation";
        case NodeType::ParenthesizedExpression:
            return "ParenthesizedExpression";
        case NodeType::FunctionCall:
            return "FunctionCall";
        case NodeType::ArrayIndex:
            return "ArrayIndex";
        default:
            return "Unknown";
        }
    }

    static void SyntaxNodeToString(std::stringstream& ss, const SyntaxNode& node, std::uint32_t depth)
    {
        ss << std::string(depth, ' ') << NodeTypeToString(node.Type);
        if (node.Content.has_value())
        {
            ss << " " << node.Content.value().ToString();
        }

        if (node.Children.size() == 0)
        {
            ss << "\n";
            return;
        }

        ss << " {\n";
        for (const auto& child : node.Children)
        {
            SyntaxNodeToString(ss, child, depth + 1);
        }
        ss << std::string(depth, ' ') << "}\n";
    }

    std::string SyntaxNode::ToString() const
    {
        std::stringstream ss;
        SyntaxNodeToString(ss, *this, 0);
        return ss.str();
    }
}
