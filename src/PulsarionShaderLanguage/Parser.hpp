#pragma once

#include "Core.hpp"
#include "Lexer.hpp"
#include "AbstractSyntaxTree.hpp"

#include <string>
#include <optional>
#include <list>
#include <vector>

namespace Pulsarion::Shader
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
    };

    /// <summary>
    /// Represents a parser error.
    /// </summary>
    struct ParserError
    {
        /// <summary>
        /// The source location of the error, from within the parser.
        /// Scope means the error was found by the scope parser, statement means the error was found by the statement parser, etc.
        /// </summary>
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
        };

        SourceLocation Location;
        ErrorSeverity Severity;
        ErrorType Type;
        ErrorSource Source;
        std::size_t NestingLevel; // Nested errors are errors that are caused by other errors, this is the nesting level of the error

        ParserError(const SourceLocation &location, ErrorSource source, ErrorSeverity severity, ErrorType type)
            : Location(location), Severity(severity), Type(type), Source(source), NestingLevel(0)
        {
        }


        static std::string ErrorSourceToString(ErrorSource source)
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
            default:
                return "Unknown";
            }
        }

        static std::string ErrorTypeToString(ErrorType type)
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
            default:
                return "Unknown";
            }
        }
    };

    /// <summary>
    /// A syntax parser for the Pulsarion Shader Language.
    /// </summary>
    class PULSARION_SHADER_LANGUAGE_API Parser
    {
    public:
    // =====================================================================================================================
    // Boilerplate
    // =====================================================================================================================

        /// <summary>
        /// Creates a new parser from a lexer.
        /// </summary>
        explicit Parser(Lexer&& lexer);
        ~Parser() = default;
        Parser(const Parser&) = delete;
        Parser(Parser&&) = delete;

    // =====================================================================================================================
    // Parsing
    // =====================================================================================================================

        struct ParseResult
        {
            std::optional<SyntaxNode> Root;
            std::list<ParserError> Errors;
            std::list<ParserError> Warnings;

            ParseResult(std::optional<SyntaxNode> root, std::list<ParserError> errors, std::list<ParserError> warnings)
                : Root(root), Errors(errors), Warnings(warnings)
            {
            }

            bool Success() const
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

            ParseResult() : Root(std::nullopt) {}
        };


        /// <summary>
        /// Parses the entire shader source code.
        /// </summary>
        /// <returns>Result of the parse</returns>
        ParseResult Parse();

        /// <summary>
        /// Parses a scope.
        /// Does not expect an opening brace, expects a closing brace and consumes it.
        /// Errors Codes:
        /// 0x00000001 - Expected a closing brace (reached EOF without finding one, but still returns a result).
        /// 0x00000002 - More than one closing brace not found (Nested scopes are also missing closing braces).
        /// </summary>
        /// <returns>The parse result, see function description for error codes</returns>
        ParseResult ParseScope();

        /// <summary>
        /// Parses a single statement. This is usually a variable declaration or a function call, or something between semicolons.
        /// </summary>
        /// <returns>The result of the statement parse</returns>
        ParseResult ParseStatement();

        /// <summary>
        /// Parses a single expression.
        /// </summary>
        /// <returns>The result of the expression parse</returns>
        ParseResult ParseExpression();

        ParseResult ParseIdentifier(bool allowNamespace = true, bool allowTrailing = true);
        ParseResult ParseAssignment();
        ParseResult ParseDeclaration();
        ParseResult ParseAnnotation(); // This is widely used in the language, so it's a separate function
        ParseResult ParseFunction(); // This is used to parse definitions
        ParseResult ParseDeclarations(); // Parses declarations for structs and functions
        ParseResult ParseStruct(); // Parses a struct definition

        // TOOD: Document Functions

        struct ExpressionParseResult
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
            std::list<ParserError> Errors;
            std::list<ParserError> Warnings;

            explicit ExpressionParseResult(const PrimType type, const std::optional<SyntaxNode> &root = std::nullopt,
                                  std::list<ParserError>&& errors ={}, const std::list<ParserError>&& warnings = {})
                : Type(type), Root(root), Errors(errors), Warnings(warnings)
            {
            }

            bool Success() const
            {
                if (!Errors.empty())
                    return false;

                PULSARION_ASSERT(Root.has_value(), "There are no errors but the Root node has no value!");
                return true;
            }
            bool IsBoolean() const { return Type == PrimType::Boolean || Type == PrimType::Comparison || Type == PrimType::Undetermined; }
            bool IsNumeric() const { return Type == PrimType::Numeric || Type == PrimType::Undetermined; }
            bool CanConvert(const PrimType other) const
            {
                if (Type == PrimType::Failed || other == PrimType::Failed)
                    return false;
                if (Type == PrimType::Undetermined || other == PrimType::Undetermined)
                    return true;
                if (Type == PrimType::Comparison && other == PrimType::Boolean || other == PrimType::Comparison && Type == PrimType::Boolean)
                    return true;
                return Type == other;
            }

            bool operator==(const ExpressionParseResult& other) const { return Type == other.Type && Root == other.Root; }
        };

        ExpressionParseResult ParseExpression(std::uint32_t minPrecedence);
        ExpressionParseResult ParseUnaryExpression();
        ExpressionParseResult ParsePrimaryExpression();

    private:
        struct InternalParseState;
        struct BacktrackState;

        [[nodiscard]] inline bool ShouldReturnStatement(ParseResult& result, bool requireEOS, InternalParseState& state, BacktrackState& backtrackState);

        struct ErrorState
        {
            std::list<ParserError> Errors;
        };

        /// <summary>
        /// This is a class that allows the parser to backtrack to a previous state.
        /// </summary>

        class LexerState
        {

        public:
            explicit LexerState(class Lexer&& lexer)
                : CurrentTokenIndex(0), Lexer(std::move(lexer)), EndOfStreamIndex(0xFFFFFFFF) // An arbitrary large number
            {
            }

            Token Peek(std::size_t offset = 0);
            Token Read();
            void Consume(std::size_t count = 1);
            bool Consume(TokenType type);
            bool Consume(TokenType type, Token& token);
            void Backtrack(std::size_t count = 1);
            void BacktrackTo(std::size_t index);
            void GoTo(std::size_t index);
            BacktrackState Snapshot();

            ~LexerState() = default;
            LexerState(const LexerState&) = delete;
            LexerState(LexerState&&) = delete;

            std::size_t CurrentTokenIndex;
        private:
            void ReadTokens(std::size_t count);

            Lexer Lexer;
            std::vector<Token> TokensRead;
            std::size_t EndOfStreamIndex;
        };


        ErrorState m_ErrorState;
        LexerState m_LexerState;
    };
}
