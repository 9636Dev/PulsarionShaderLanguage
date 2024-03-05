#pragma once

#include "Core.hpp"
#include "Lexer.hpp"
#include "ParserUtil.hpp"

#include <vector>

namespace Pulsarion::Shader
{


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

        /// <summary>
        /// Parses the entire shader source code.
        /// </summary>
        /// <returns>Result of the parse</returns>
        Parsing::Result Parse();

        /// <summary>
        /// Parses a scope.
        /// Does not expect an opening brace, expects a closing brace and consumes it.
        /// </summary>
        /// <returns>The parse result, see function description for error codes</returns>
        Parsing::Result ParseScope();

        /// <summary>
        /// Parses a single statement. This is usually a variable declaration or a function call, or something between semicolons.
        /// </summary>
        /// <returns>The result of the statement parse</returns>
        Parsing::Result ParseStatement();

        /// <summary>
        /// Parses a single expression.
        /// </summary>
        /// <returns>The result of the expression parse</returns>
        Parsing::Result ParseExpression();

        Parsing::Result ParseIdentifier(bool allowNamespace = true, bool allowTrailing = true);
        Parsing::Result ParseAssignment();
        Parsing::Result ParseDeclaration();
        Parsing::Result ParseAnnotation(); // This is widely used in the language, so it's a separate function
        Parsing::Result ParseFunction(); // This is used to parse definitions
        Parsing::Result ParseDeclarations(); // Parses declarations for structs and functions
        Parsing::Result ParseStruct(); // Parses a struct definition
        std::pair<Parsing::Result, bool> ParseKeywords(); // Parses keywords like 'if', 'else', 'for', 'while', etc.

        // TOOD: Document Functions


        Parsing::ExpressionResult ParseExpression(std::uint32_t minPrecedence);
        Parsing::ExpressionResult ParseUnaryExpression();
        Parsing::ExpressionResult ParsePrimaryExpression();

    private:
        struct InternalParseState;
        struct BacktrackState;

        [[nodiscard]] inline bool ShouldReturnStatement(Parsing::Result& result, bool requireEOS, InternalParseState& state, BacktrackState& backtrackState);

        /// <summary>
        /// This is a class that allows the parser to backtrack to a previous state.
        /// </summary>

        class LexerState
        {

        public:
            explicit LexerState(Lexer&& lexer)
                : CurrentTokenIndex(0), Lexer(lexer), EndOfStreamIndex(0xFFFFFFFF) // An arbitrary large number
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


        LexerState m_LexerState;
    };
}
