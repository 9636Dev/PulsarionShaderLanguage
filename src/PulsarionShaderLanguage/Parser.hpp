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
        /// Does not expect an opening brace.
        /// Expects a closing brace and consumes it. (Error 0x0000 0001 if not found)
        /// </summary>
        /// <returns>
        /// ErrorFlags:
        /// 0x0000 0001 - Missing closing brace
        /// </returns>
        ParseResult ParseScope();

    private:
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
