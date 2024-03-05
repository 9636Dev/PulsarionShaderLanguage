#include "AbstractSyntaxTree.hpp"

#include <sstream>
#include <stack>

namespace Pulsarion::Shader
{
    std::string NodeTypeToString(NodeType type)
    {
        switch (type)
        {
        case NodeType::Root:
            return "Root";
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
        case NodeType::IdentifierTrailing:
            return "IdentifierTrailing";
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
        case NodeType::ReturnNoValue:
            return "ReturnNoValue";
        case NodeType::ReturnExpression:
            return "ReturnExpression";
        case NodeType::Break:
            return "Break";
        case NodeType::Continue:
            return "Continue";
        case NodeType::If:
            return "If";
        case NodeType::Else:
            return "Else";
        case NodeType::While:
            return "While";
        case NodeType::For:
            return "For";
        case NodeType::DoWhile:
            return "DoWhile";
        case NodeType::TernaryOperation:
            return "TernaryOperation";
        default:
            return "Unknown";
        }
    }

    static void SyntaxNodeToString(std::stringstream& ss, const SyntaxNode& node, const std::uint32_t depth) // NOLINT(*-no-recursion)
    {
        ss << std::string(depth, ' ') << NodeTypeToString(node.Type);
        if (node.Content.has_value())
        {
            ss << " " << node.Content.value().ToString();
        }

        if (node.Children.empty())
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


    void AbstractSyntaxTree::Traverse(const TraversalFunction& callback)
    {
        std::stack<std::pair<SyntaxNode*, size_t>> stack; // Pair of node pointer and child index

        // Start with the root node and index 0 (indicating no children have been processed yet)
        stack.emplace(&m_Root, 0);

        while (!stack.empty())
        {
            auto& [currentNode, childIndex] = stack.top();

            // If this is the first time visiting this node, invoke onNodeVisit with `TraversalPhase::Advance`
            auto res = callback(*currentNode, TraversalPhase::Advance);
            if (childIndex == 0 && res.StopExploringSubtree)
            {
                stack.pop(); // Stop processing this node and its subtree
                continue;
            }
            childIndex += res.SkipChildren; // How many children to skip

            // If all children of this node have been processed, or if it has no children
            if (childIndex >= currentNode->Children.size())
            {
                auto res = callback(*currentNode, TraversalPhase::Return);
                // Currently the result is ignored, but it could be used to stop the traversal
                stack.pop(); // Move back up the tree
                continue;
            }

            // Process the next child
            auto& nextChild = currentNode->Children[childIndex];
            // Before diving into the child, increment the childIndex for the current node for the next iteration
            stack.top().second++;
            // Add the child to the stack to process it in the next iteration
            stack.emplace(&nextChild, 0);
        }
    }
}
