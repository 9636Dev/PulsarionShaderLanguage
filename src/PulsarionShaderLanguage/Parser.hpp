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
            Struct
        };

        SourceLocation Location;
        ErrorSeverity Severity;
        ErrorSource Source;
        std::string Message;
        std::uint32_t ErrorFlagSet; // This is useful to clear only the errors that were caused by a specific error

        ParserError(SourceLocation location, ErrorSource source, ErrorSeverity severity, std::string message, std::uint32_t errorFlagSet = 0u)
            : Location(location), Source(source), Severity(severity), Message(message), ErrorFlagSet(errorFlagSet)
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
        Parser(Lexer&& lexer);
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
            std::uint32_t ErrorFlags;
            /// <summary>
            /// Whether the parser recovered from an error.
            /// If it was recovered, then the parser can continue parsing, but the error should be reported.
            /// </summary>
            bool WasRecovered;

            ParseResult(std::optional<SyntaxNode> root, std::list<ParserError> errors, std::uint32_t errorFlags, bool wasRecovered = false)
                : Root(root), Errors(errors), ErrorFlags(errorFlags), WasRecovered(wasRecovered)
            {
            }

            ParseResult() : Root(std::nullopt), Errors(), ErrorFlags(0u), WasRecovered(false) {}
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
            std::uint64_t ErrorFlags;

            ExpressionParseResult(PrimType type, std::optional<SyntaxNode> root = std::nullopt, std::list<ParserError> errors = {}, std::uint64_t errorFlags = 0ull)
                : Type(type), Root(root), Errors(errors), ErrorFlags(errorFlags)
            {
            }

            bool IsBoolean() const { return Type == PrimType::Boolean || Type == PrimType::Comparison || Type == PrimType::Undetermined; }
            bool IsNumeric() const { return Type == PrimType::Numeric || Type == PrimType::Undetermined; }
            bool CanConvert(PrimType other) const
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
            LexerState(class Lexer&& lexer)
                : Lexer(std::move(lexer)), TokensRead(), CurrentTokenIndex(0), EndOfStreamIndex(0xFFFFFFFF) // An arbitrary large number
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
