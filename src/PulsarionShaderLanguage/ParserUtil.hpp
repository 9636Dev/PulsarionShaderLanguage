#pragma once

#include "Core.hpp"
#include "AbstractSyntaxTree.hpp"

#include <optional>
#include <list>

namespace Pulsarion::Shader::Parsing
{

    // We have one error type for every time of error even if it is only slightly different.
    enum class ErrorType
    {
        UnexpectedEndOfFileWhenFindingClosingBrace,
        UnexpectedExtraClosingBrace,
        ExpectedEndOfStatement, // you forgot to put a semicolon at the end of a statement
        ExpectedIdentifierForReturnType,
        ExpectedOpeningParenthesisForFunctionArguments,
        ExpectedClosingParenthesisForFunctionArguments,
        ExpectedIdentifierForArgumentType,
        ExpectedIdentifierForIdentifier,
        UnexpectedDotInNonTrailingIdentifier,
        ExpectedStructKeyword,
        ExpectedIdentifierForStructName,
        ExpectedOpeningBraceForFunctionBody,
        VariableDefinitionNotAllowedInStruct,
        FunctionDeclarationNotAllowedInStruct,
        StructDeclarationNotAllowedInStruct,
        StructDefinitionNotAllowedInStruct,
        UnexpectedNodeTypeInStruct,
        ExpectedIdentifierForFunctionName,
        ExpectedIdentifierForArgumentName,
        ExpectedScopeForStructDefinition,
        ExpectedScopeForFunctionBody,
        ExpectedIdentifierForAssignment,
        ExpectedAssignmentOperator,
        ExpectedExpressionForAssignment,
        ExpectedIdentifierForVariableDeclaration,
        ExpectedIdentifierForVariableName,
        ExpectedTypeForInitializerListVariableDeclaration,
        ExpectedExpressionForInitializerListVariableDeclaration,
        ExpectedClosingParenthesisForInitializerListVariableDeclaration,
        ExpectedExpressionForVariableDefinition,
        ExpectedDoubleLeftBracketForAnnotation,
        ExpectedIdentifierForAnnotation,
        ExpectedDoubleRightBracketForAnnotation,
        ExpressionOperatorRequiresBooleanOperands,
        ExpressionComparisonOperatorRequiresCompatibleOperands,
        ExpressionOperatorRequiresNumericOperands,
        ExpressionExpectsNumericPrimType,
        ExpressionExpectsBooleanPrimType,
        ExpressionExpectsClosingParenthesis,
        ExpectedExpressionInTemplateArgumentList,
        ExpectedClosingAngleBracketForTemplateArgumentList,
        ExpectedExpressionInFunctionCallArgumentList,
        ExpectedClosingParenthesisForFunctionCallArgumentList,
        ExpectedNumericExpressionForArrayAccess,
        ExpectedClosingBracketForArrayAccess,
        ExpectedExpressionForReturnStatement,
        ExpectedKeyword,
        ExpectedExpression,
        ExpectedLeftParenthesisForIfCondition,
        ExpectedRightParenthesisForIfCondition,
        ExpectedLeftParenthesisForWhileCondition,
        ExpectedRightParenthesisForWhileCondition,
        ExpectedLeftParenthesisForForCondition,
        ExpectedRightParenthesisForForCondition,
        ExpectedLeftParenthesisForSwitchCondition,
        ExpectedRightParenthesisForSwitchCondition,
        ExpectedLeftBraceForSwitchBody,
        ExpectedRightBraceForSwitchBody,
        ExpectedLeftBraceForDoBody,
        ExpectedRightBraceForDoBody,
        ExpectedSemicolonForDoWhile,
        ExpectedLeftBraceForElseBody,
        ExpectedRightBraceForElseBody,
        ExpectedLeftBraceForIfBody,
        ExpectedRightBraceForIfBody,
        ExpectedLeftBraceForWhileBody,
        ExpectedRightBraceForWhileBody,
        ExpectedLeftBraceForForBody,
        ExpectedRightBraceForForBody,
        ExpectedColonForSwitchCase,
        ExpectedScopeForIfBody,
        ExpectedStatementForIfBody,
        ExpectedColonForTernaryOperator,
        TernaryOperatorRequiresCompatibleOperands,
        TernaryOperatorRequiresBooleanCondition,
        ExpectedOpeningParenthesisForFunctionCall,
    };

    enum class ErrorSource
    {
        Scope,
        Statement,
        Expression,
        Identifier,
        Assignment,
        Annotation,
        Function,
        Struct,
        Declaration,
        Keyword,
        KeywordReturn,
        KeywordIf,
        KeywordElse,
        KeywordFor,
        KeywordWhile,
        KeywordDo,
        KeywordSwitch,
    };

    /// <summary>
    /// Represents a parser error.
    /// </summary>
    struct PULSARION_SHADER_LANGUAGE_API Error
    {
        /// <summary>
        /// The source location of the error, from within the parser.
        /// Scope means the error was found by the scope parser, statement means the error was found by the statement parser, etc.
        /// </summary>

        SourceLocation Location;
        ErrorSeverity Severity;
        ErrorType Type;
        ErrorSource Source;
        std::size_t NestingLevel; // Nested errors are errors that are caused by other errors, this is the nesting level of the error

        Error(const SourceLocation &location, ErrorSource source, ErrorSeverity severity, ErrorType type)
            : Location(location), Severity(severity), Type(type), Source(source), NestingLevel(0)
        {
        }


        static std::string SourceToString(ErrorSource source);
        static std::string TypeToString(ErrorType type);
    };

    struct Result
    {
        std::optional<SyntaxNode> Root;
        std::list<Error> Errors;
        std::list<Error> Warnings;

        Result(std::optional<SyntaxNode>&& root, std::list<Error>&& errors, std::list<Error>&& warnings)
            : Root(root), Errors(errors), Warnings(warnings)
        {
        }

        [[nodiscard]] bool Success() const
        {
            if (!Errors.empty())
                return false;

            PULSARION_ASSERT(Root.has_value(), "There are no errors but the Root node has no value!");
            return true;
        }

        void Nest()
        {
            for (auto& error : Errors)
                error.NestingLevel++;
            for (auto& warning : Warnings)
                warning.NestingLevel++;
        }

        Result() : Root(std::nullopt) {}
    };


    struct ExpressionResult
    {
        enum class PrimType
        {
            Boolean,
            Numeric,
            Comparison,
            Undetermined, // This is the case for function calls and variables
            Failed, // This is the case for failed parses
        };

        PrimType Type;
        std::optional<SyntaxNode> Root;
        std::list<Parsing::Error> Errors;
        std::list<Parsing::Error> Warnings;

        explicit ExpressionResult(const PrimType type, const std::optional<SyntaxNode> &root = std::nullopt,
                              std::list<Parsing::Error>&& errors ={}, const std::list<Parsing::Error>&& warnings = {})
            : Type(type), Root(root), Errors(errors), Warnings(warnings)
        {
        }

        [[nodiscard]] bool Success() const
        {
            if (!Errors.empty())
                return false;

            PULSARION_ASSERT(Root.has_value(), "There are no errors but the Root node has no value!");
            return true;
        }
        [[nodiscard]] bool IsBoolean() const { return Type == PrimType::Boolean || Type == PrimType::Comparison || Type == PrimType::Undetermined; }
        [[nodiscard]] bool IsNumeric() const { return Type == PrimType::Numeric || Type == PrimType::Undetermined; }
        [[nodiscard]] bool CanConvert(const PrimType other) const
        {
            if (Type == PrimType::Failed || other == PrimType::Failed)
                return false;
            if (Type == PrimType::Undetermined || other == PrimType::Undetermined)
                return true;
            if (Type == PrimType::Comparison && other == PrimType::Boolean || other == PrimType::Comparison && Type == PrimType::Boolean)
                return true;
            return Type == other;
        }

        bool operator==(const ExpressionResult& other) const { return Type == other.Type && Root == other.Root; }
    };

    struct Identifier
    {
        std::vector<std::string> Namespace;
        std::string Name;
        std::vector<std::string> Trailing; // This is for a.b, where b will be trailing
        // TODO: Support template arguments

        Identifier() = default;
    };

    enum class VariablePrimType
    {
        Int,
        UInt,
        Long,
        ULong,
        LongLong,
        ULongLong,
        Float,
        Double,
        Bool,
        Void,
        Struct,
        Unknown,
    };

    struct VariableType
    {
        VariablePrimType Type;
        // Not present if the type is not an array, for now we only support arrays of fixed size
        std::optional<std::size_t> ArraySize;
    };

    struct FunctionType
    {
        struct Argument
        {
            Identifier Name;
            VariableType Type;
        };

        VariableType ReturnType;
        std::vector<Argument> Arguments;
    };

    struct StructType
    {
        std::unordered_map<Identifier, VariableType> Variables;
        std::unordered_map<Identifier, FunctionType> Functions;
    };

    struct TypeInfo
    {
        std::unordered_map<Identifier, VariableType> Variables;
        std::unordered_map<Identifier, FunctionType> Functions;
        std::unordered_map<Identifier, StructType> Structs;
    };

    PULSARION_SHADER_LANGUAGE_API std::optional<Identifier> ParseIdentifierFromNode(const SyntaxNode& node, const std::vector<std::string>& namespaces);
}
