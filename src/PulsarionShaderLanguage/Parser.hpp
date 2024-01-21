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
            Expression
        };

        SourceLocation Location;
        ErrorSeverity Severity;
        ErrorSource Source;
        std::string Message;
        std::uint32_t ErrorFlagSet; // This is useful to clear only the errors that were caused by a specific error

        ParserError(SourceLocation location, ErrorSource source, ErrorSeverity severity, std::string message, std::uint32_t errorFlagSet)
            : Location(location), Source(source), Severity(severity), Message(message), ErrorFlagSet(errorFlagSet)
        {
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
        };


        /// <summary>
        /// Parses the shader source code.
        /// </summary>
        ParseResult Parse();

        /// <summary>
        /// Parses a scope.
        /// Does not expect an opening brace, expects a closing brace and consumes it.
        /// Errors:
        /// 0x00000001 - Expected a closing brace (reached EOF without finding one).
        /// 0x00000002 - More than one closing brace not found (Nested scopes are also missing closing braces).
        /// </summary>
        ParseResult ParseScope();

    private:
        struct InternalParseState;
        struct ErrorState
        {
            std::list<ParserError> Errors;
        };

        struct LexerState
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

            ~LexerState() = default;
            LexerState(const LexerState&) = delete;
            LexerState(LexerState&&) = delete;

        private:
            void ReadTokens(std::size_t count);

            Lexer Lexer;
            std::vector<Token> TokensRead;
            std::size_t CurrentTokenIndex;
            std::size_t EndOfStreamIndex;
        };

        ErrorState m_ErrorState;
        LexerState m_LexerState;
    };
}
