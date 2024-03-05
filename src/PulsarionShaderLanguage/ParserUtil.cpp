#include "ParserUtil.hpp"

#include <unordered_map>

namespace Pulsarion::Shader::Parsing
{
    std::string Error::SourceToString(ErrorSource source)
    {
        switch (source)
        {
        case ErrorSource::Scope:
            return "Scope";
        case ErrorSource::Statement:
            return "Statement";
        case ErrorSource::Expression:
            return "Expression";
        case ErrorSource::Identifier:
            return "Identifier";
        case ErrorSource::Assignment:
            return "Assignment";
        case ErrorSource::Annotation:
            return "Annotation";
        case ErrorSource::Function:
            return "Function";
        case ErrorSource::Struct:
            return "Struct";
        case ErrorSource::Declaration:
            return "Declaration";
        case ErrorSource::Keyword:
            return "Keyword";
        case ErrorSource::KeywordReturn:
            return "KeywordReturn";
        case ErrorSource::KeywordIf:
            return "KeywordIf";
        case ErrorSource::KeywordElse:
            return "KeywordElse";
        case ErrorSource::KeywordFor:
            return "KeywordFor";
        case ErrorSource::KeywordWhile:
            return "KeywordWhile";
        case ErrorSource::KeywordDo:
            return "KeywordDo";
        case ErrorSource::KeywordSwitch:
            return "KeywordSwitch";
        default:
            return "Unknown";
        }
    }

    std::string Error::TypeToString(ErrorType type)
    {
        switch (type)
        {
        case ErrorType::UnexpectedEndOfFileWhenFindingClosingBrace:
            return "UnexpectedEndOfFileWhenFindingClosingBrace";
        case ErrorType::UnexpectedExtraClosingBrace:
            return "UnexpectedExtraClosingBrace";
        case ErrorType::ExpectedEndOfStatement:
            return "ExpectedEndOfStatement";
        case ErrorType::ExpectedIdentifierForReturnType:
            return "ExpectedIdentifierForReturnType";
        case ErrorType::ExpectedOpeningParenthesisForFunctionArguments:
            return "ExpectedOpeningParenthesisForFunctionArguments";
        case ErrorType::ExpectedClosingParenthesisForFunctionArguments:
            return "ExpectedClosingParenthesisForFunctionArguments";
        case ErrorType::ExpectedIdentifierForArgumentType:
            return "ExpectedIdentifierForArgumentType";
        case ErrorType::ExpectedIdentifierForIdentifier:
            return "ExpectedIdentifierForIdentifier";
        case ErrorType::UnexpectedDotInNonTrailingIdentifier:
            return "UnexpectedDotInNonTrailingIdentifier";
        case ErrorType::ExpectedStructKeyword:
            return "ExpectedStructKeyword";
        case ErrorType::ExpectedIdentifierForStructName:
            return "ExpectedIdentifierForStructName";
        case ErrorType::ExpectedOpeningBraceForFunctionBody:
            return "ExpectedOpeningBraceForFunctionBody";
        case ErrorType::VariableDefinitionNotAllowedInStruct:
            return "VariableDefinitionNotAllowedInStruct";
        case ErrorType::FunctionDeclarationNotAllowedInStruct:
            return "FunctionDeclarationNotAllowedInStruct";
        case ErrorType::StructDeclarationNotAllowedInStruct:
            return "StructDeclarationNotAllowedInStruct";
        case ErrorType::StructDefinitionNotAllowedInStruct:
            return "StructDefinitionNotAllowedInStruct";
        case ErrorType::UnexpectedNodeTypeInStruct:
            return "UnexpectedNodeTypeInStruct";
        case ErrorType::ExpectedIdentifierForFunctionName:
            return "ExpectedIdentifierForFunctionName";
        case ErrorType::ExpectedIdentifierForArgumentName:
            return "ExpectedIdentifierForArgumentName";
        case ErrorType::ExpectedScopeForStructDefinition:
            return "ExpectedScopeForStructDefinition";
        case ErrorType::ExpectedScopeForFunctionBody:
            return "ExpectedScopeForFunctionBody";
        case ErrorType::ExpectedIdentifierForAssignment:
            return "ExpectedIdentifierForAssignment";
        case ErrorType::ExpectedAssignmentOperator:
            return "ExpectedAssignmentOperator";
        case ErrorType::ExpectedExpressionForAssignment:
            return "ExpectedExpressionForAssignment";
        case ErrorType::ExpectedIdentifierForVariableDeclaration:
            return "ExpectedIdentifierForVariableDeclaration";
        case ErrorType::ExpectedIdentifierForVariableName:
            return "ExpectedIdentifierForVariableName";
        case ErrorType::ExpectedTypeForInitializerListVariableDeclaration:
            return "ExpectedTypeForInitializerListVariableDeclaration";
        case ErrorType::ExpectedExpressionForInitializerListVariableDeclaration:
            return "ExpectedExpressionForInitializerListVariableDeclaration";
        case ErrorType::ExpectedClosingParenthesisForInitializerListVariableDeclaration:
            return "ExpectedClosingParenthesisForInitializerListVariableDeclaration";
        case ErrorType::ExpectedExpressionForVariableDefinition:
            return "ExpectedExpressionForVariableDefinition";
        case ErrorType::ExpectedDoubleLeftBracketForAnnotation:
            return "ExpectedDoubleLeftBracketForAnnotation";
        case ErrorType::ExpectedIdentifierForAnnotation:
            return "ExpectedIdentifierForAnnotation";
        case ErrorType::ExpectedDoubleRightBracketForAnnotation:
            return "ExpectedDoubleRightBracketForAnnotation";
        case ErrorType::ExpressionOperatorRequiresBooleanOperands:
            return "ExpressionOperatorRequiresBooleanOperands";
        case ErrorType::ExpressionComparisonOperatorRequiresCompatibleOperands:
            return "ExpressionComparisonOperatorRequiresCompatibleOperands";
        case ErrorType::ExpressionOperatorRequiresNumericOperands:
            return "ExpressionOperatorRequiresNumericOperands";
        case ErrorType::ExpressionExpectsNumericPrimType:
            return "ExpressionExpectsNumericPrimType";
        case ErrorType::ExpressionExpectsBooleanPrimType:
            return "ExpressionExpectsBooleanPrimType";
        case ErrorType::ExpressionExpectsClosingParenthesis:
            return "ExpressionExpectsClosingParenthesis";
        case ErrorType::ExpectedExpressionInTemplateArgumentList:
            return "ExpectedExpressionInTemplateArgumentList";
        case ErrorType::ExpectedClosingAngleBracketForTemplateArgumentList:
            return "ExpectedClosingAngleBracketForTemplateArgumentList";
        case ErrorType::ExpectedExpressionInFunctionCallArgumentList:
            return "ExpectedExpressionInFunctionCallArgumentList";
        case ErrorType::ExpectedClosingParenthesisForFunctionCallArgumentList:
            return "ExpectedClosingParenthesisForFunctionCallArgumentList";
        case ErrorType::ExpectedNumericExpressionForArrayAccess:
            return "ExpectedNumericExpressionForArrayAccess";
        case ErrorType::ExpectedClosingBracketForArrayAccess:
            return "ExpectedClosingBracketForArrayAccess";
        case ErrorType::ExpectedExpressionForReturnStatement:
            return "ExpectedExpressionForReturnStatement";
        case ErrorType::ExpectedKeyword:
            return "ExpectedKeyword";
        case ErrorType::ExpectedExpression:
            return "ExpectedExpression";
        case ErrorType::ExpectedLeftParenthesisForIfCondition:
            return "ExpectedLeftParenthesisForIfCondition";
        case ErrorType::ExpectedRightParenthesisForIfCondition:
            return "ExpectedRightParenthesisForIfCondition";
        case ErrorType::ExpectedLeftParenthesisForWhileCondition:
            return "ExpectedLeftParenthesisForWhileCondition";
        case ErrorType::ExpectedRightParenthesisForWhileCondition:
            return "ExpectedRightParenthesisForWhileCondition";
        case ErrorType::ExpectedLeftParenthesisForForCondition:
            return "ExpectedLeftParenthesisForForCondition";
        case ErrorType::ExpectedRightParenthesisForForCondition:
            return "ExpectedRightParenthesisForForCondition";
        case ErrorType::ExpectedLeftParenthesisForSwitchCondition:
            return "ExpectedLeftParenthesisForSwitchCondition";
        case ErrorType::ExpectedRightParenthesisForSwitchCondition:
            return "ExpectedRightParenthesisForSwitchCondition";
        case ErrorType::ExpectedLeftBraceForSwitchBody:
            return "ExpectedLeftBraceForSwitchBody";
        case ErrorType::ExpectedRightBraceForSwitchBody:
            return "ExpectedRightBraceForSwitchBody";
        case ErrorType::ExpectedLeftBraceForDoBody:
            return "ExpectedLeftBraceForDoBody";
        case ErrorType::ExpectedRightBraceForDoBody:
            return "ExpectedRightBraceForDoBody";
        case ErrorType::ExpectedSemicolonForDoWhile:
            return "ExpectedSemicolonForDoWhile";
        case ErrorType::ExpectedLeftBraceForElseBody:
            return "ExpectedLeftBraceForElseBody";
        case ErrorType::ExpectedRightBraceForElseBody:
            return "ExpectedRightBraceForElseBody";
        case ErrorType::ExpectedLeftBraceForIfBody:
            return "ExpectedLeftBraceForIfBody";
        case ErrorType::ExpectedRightBraceForIfBody:
            return "ExpectedRightBraceForIfBody";
        case ErrorType::ExpectedLeftBraceForWhileBody:
            return "ExpectedLeftBraceForWhileBody";
        case ErrorType::ExpectedRightBraceForWhileBody:
            return "ExpectedRightBraceForWhileBody";
        case ErrorType::ExpectedLeftBraceForForBody:
            return "ExpectedLeftBraceForForBody";
        case ErrorType::ExpectedRightBraceForForBody:
            return "ExpectedRightBraceForForBody";
        case ErrorType::ExpectedColonForSwitchCase:
            return "ExpectedColonForSwitchCase";
        case ErrorType::ExpectedScopeForIfBody:
            return "ExpectedScopeForIfBody";
        case ErrorType::ExpectedStatementForIfBody:
            return "ExpectedStatementForIfBody";
        case ErrorType::ExpectedColonForTernaryOperator:
            return "ExpectedColonForTernaryOperator";
        case ErrorType::TernaryOperatorRequiresCompatibleOperands:
            return "TernaryOperatorRequiresCompatibleOperands";
        case ErrorType::TernaryOperatorRequiresBooleanCondition:
            return "TernaryOperatorRequiresBooleanCondition";
        case ErrorType::ExpectedOpeningParenthesisForFunctionCall:
            return "ExpectedOpeningParenthesisForFunctionCall";
        default:
            return "Unknown";
        }
    }

    VariablePrimType ParseVariablePrimType(const SyntaxNode& node)
    {
        if (!node.Children.empty())
            return VariablePrimType::Unknown; // PrimType nodes should not have children

        // TODO: Maybe some kind of hash instead of string comparison
        PULSARION_ASSERT(node.Content.has_value(), "Expected content for prim type node");
        if (node.Content->Value == "int")
            return VariablePrimType::Int;
        if (node.Content->Value == "float")
            return VariablePrimType::Float;
        if (node.Content->Value == "bool")
            return VariablePrimType::Bool;
        if (node.Content->Value == "void")
            return VariablePrimType::Void;
        if (node.Content->Value == "uint")
            return VariablePrimType::UInt;
        if (node.Content->Value == "double")
            return VariablePrimType::Double;
        if (node.Content->Value == "long")
            return VariablePrimType::Long;
        if (node.Content->Value == "ulong")
            return VariablePrimType::ULong;
        if (node.Content->Value == "longlong")
            return VariablePrimType::LongLong;
        if (node.Content->Value == "ulonglong")
            return VariablePrimType::ULongLong;
        return VariablePrimType::Unknown;
    }

    std::optional<Identifier> ParseIdentifierFromNode(const SyntaxNode& node, const std::vector<std::string>& namespaces)
    {
        if (node.Type != NodeType::Identifier)
            return std::nullopt;

        PULSARION_ASSERT(node.Content.has_value(), "Expected content for identifier node");
        Identifier result;
        result.Namespace = namespaces; // We just copy the current namespace
        result.Name = node.Content->Value;
        if (node.Children.empty())
            return result;

        if (node.Children.size() == 2)
        {
            // There is trailing, so we need to parse it
            auto trailing = node.Children[1];
            PULSARION_ASSERT(trailing.Type == NodeType::IdentifierTrailing, "Expected identifier trailing node");
            for (const auto& child : trailing.Children)
            {
                PULSARION_ASSERT(child.Content.has_value(), "Expected content for identifier trailing node");
                result.Trailing.push_back(child.Content->Value);
            }
        }

        if (node.Children[0].Type != NodeType::Namespace)
            return result;

        auto nmspcs = node.Children[0].Children;
        PULSARION_ASSERT(nmspcs[0].Content.has_value(), "Expected content in namespace node");

        //NOLINTNEXTLINE(bugprone-unchecked-optional-access)
        if (nmspcs[0].Content->Type == TokenType::ColonColon) // Clang-tidy is wrong here
        {
            result.Namespace.clear();
            nmspcs.erase(nmspcs.begin());
        }

        for (const auto& nms : nmspcs)
        {
            PULSARION_ASSERT(nms.Content.has_value(), "Namespace token in node does not contain value!");
            result.Namespace.push_back(nms.Content->Value);
        }

        return result;
    }
}
