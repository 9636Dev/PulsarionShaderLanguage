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
        case NodeType::Assignment:
            return "Assignment";
        case NodeType::VariableDeclaration:
            return "VariableDeclaration";
        case NodeType::VariableDefinition:
            return "VariableDefinition";
        case NodeType::VariableInitialization:
            return "VariableInitialization";
        case NodeType::ArrayDeclaration:
            return "ArrayDeclaration";
        case NodeType::ArrayDefinition:
            return "ArrayDefinition";
        case NodeType::AssignmentAdd:
            return "AssignmentAdd";
        case NodeType::AssignmentSubtract:
            return "AssignmentSubtract";
        case NodeType::AssignmentMultiply:
            return "AssignmentMultiply";
        case NodeType::AssignmentDivide:
            return "AssignmentDivide";
        case NodeType::AssignmentModulo:
            return "AssignmentModulo";
        case NodeType::AssignmentBitwiseAnd:
            return "AssignmentBitwiseAnd";
        case NodeType::AssignmentBitwiseOr:
            return "AssignmentBitwiseOr";
        case NodeType::AssignmentBitwiseXor:
            return "AssignmentBitwiseXor";
        case NodeType::AssignmentBitwiseLeftShift:
            return "AssignmentBitwiseLeftShift";
        case NodeType::AssignmentBitwiseRightShift:
            return "AssignmentBitwiseRightShift";
        case NodeType::Annotation:
            return "Annotation";
        case NodeType::KeywordAuto:
            return "KeywordAuto";
        case NodeType::ArgumentList:
            return "ArgumentList";
        case NodeType::TemplateArgumentList:
            return "TemplateArgumentList";
        case NodeType::FunctionArgument:
            return "FunctionArgument";
        case NodeType::FunctionArgumentList:
            return "FunctionArgumentList";
        case NodeType::FunctionDefinition:
            return "FunctionDefinition";
        case NodeType::FunctionDeclaration:
            return "FunctionDeclaration";
        case NodeType::StructDefinition:
            return "StructDefinition";
        case NodeType::StructDeclaration:
            return "StructDeclaration";
        case NodeType::StructMemberVariable:
            return "StructMemberVariable";
        case NodeType::StructMemberFunction:
            return "StructMemberFunction";
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
