#pragma once

#include "Core.hpp"
#include "Token.hpp"

#include <vector>
#include <optional>
#include <functional>

namespace Pulsarion::Shader
{
    enum class NodeType
    {
        Scope,
        Statement,
        Token,
        NumericLiteral,
        BooleanLiteral,
        Identifier,
        Namespace,
        BooleanNegation,
        NumericNegation,
        NumericPreIncrement,
        NumericPreDecrement,
        NumericPostIncrement,
        NumericPostDecrement,
        NumericBitwiseNot,
        NumericUnaryPlus,
        BinaryBooleanOperation,
        BinaryNumericOperation,
        BinaryComparisonOperation,
        ParenthesizedExpression,
        FunctionCall,
        ArrayIndex,
        Assignment,
        VariableDeclaration,
        VariableDefinition, // This is a variable declaration with an initializer or using an assignment operator
        VariableInitialization, // This is a variable declaration with an initializer list like in C++
        ArrayDeclaration,
        ArrayDefinition, // This is an array declaration with an initializer or using an assignment operator
        AssignmentAdd,
        AssignmentSubtract,
        AssignmentMultiply,
        AssignmentDivide,
        AssignmentModulo,
        AssignmentBitwiseAnd,
        AssignmentBitwiseOr,
        AssignmentBitwiseXor,
        AssignmentBitwiseLeftShift,
        AssignmentBitwiseRightShift,
        Annotation, // This is widely used in PSHL
        KeywordAuto,
        FunctionArgument,
        FunctionArgumentList,
        ArgumentList,
        TemplateArgumentList,
        FunctionDefinition,
        FunctionDeclaration,
        StructDefinition,
        StructDeclaration,
        StructMemberVariable,
        StructMemberFunction,
        ReturnNoValue,
        ReturnExpression,
        Break,
        Continue,
        If,
        Else,
        While,
        For,
        DoWhile,
        TernaryOperation,
    };

    PULSARION_SHADER_LANGUAGE_API std::string NodeTypeToString(NodeType type);

    struct PULSARION_SHADER_LANGUAGE_API SyntaxNode
    {
        NodeType Type;
        std::optional<Token> Content;
        SourceLocation Location;
        std::vector<SyntaxNode> Children;

        SyntaxNode(const NodeType type, const SourceLocation &location,
                   const std::optional<Token> &content = std::nullopt,
                   const std::vector<SyntaxNode> &children = std::vector<SyntaxNode>())
            : Type(type), Content(content), Location(location), Children(children)
        {
        }

        explicit SyntaxNode(Token token)
            : Type(NodeType::Token), Content(token), Location(token.Location)
        {
        }

        [[nodiscard]] std::string ToString() const;

        bool operator==(const SyntaxNode& other) const
        {
            return Type == other.Type && Content == other.Content && Location == other.Location && Children == other.Children;
        }
    };

    enum class TraversalPhase
    {
        Advance, // When it advances to the next node
        Return, // When it goes back to the parent node
        Deepen, // When it explores the children of the current node
    };

    class PULSARION_SHADER_LANGUAGE_API AbstractSyntaxTree
    {
    public:
        explicit AbstractSyntaxTree(SyntaxNode& root) : m_Root(root) {}

        void Traverse(const std::function<bool(SyntaxNode&, TraversalPhase)>& callback);

    private:
        SyntaxNode& m_Root;
    };
}
