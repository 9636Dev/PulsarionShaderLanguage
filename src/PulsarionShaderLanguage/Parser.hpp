#pragma once

#include "Core.hpp"
#include "Lexer.hpp"
#include "AbstractSyntaxTree.hpp"

#include <string>
#include <optional>
#include <stack>

namespace Pulsarion::Shader
{
    /// <summary>
    /// Represents a location in the source code.
    /// </summary>
    struct SourceLocation
    {
        /// <summary>
        /// The line number.
        /// </summary>
        std::size_t Line;

        /// <summary>
        /// The column number.
        /// </summary>
        std::size_t Column;

        /// <summary>
        /// The character index.
        /// <summary>
        std::size_t Index;

        /// <summary>
        /// Length of the source location.
        /// </summary>
        std::size_t Length;
    };

    /// <summary>
    /// Represents a parser error.
    /// </summary>
    struct ParserError
    {
        SourceLocation Location;
        ErrorSeverity Severity;
        std::string Message;
        /// <summary>
        /// Sometimes errors are caused by other errors.
        /// </summary>
        std::optional<ParserError> Parent;
    };

    class PULSARION_SHADER_LANGUAGE_API Parser
    {
    public:
        Parser(Lexer&& lexer);
        ~Parser();

        Token PeekToken(std::size_t n = 0) const; // We look at the current token, which is the next token to be read.
        Token ReadToken(); // We read the current token.
        bool ConsumeToken(TokenType type); // We consume the current token if it is of the given type, doesn't consume the token if it fails.
        void Backtrack(std::size_t n = 1); // We go back n tokens.

    private:
        struct ReadState
        {
            mutable std::vector<Token> ReadTokens;
            std::size_t CurrentIndex;

            ReadState() : ReadTokens(), CurrentIndex(0) {}
        };

        mutable Lexer m_Lexer; // We will take ownership of the lexer.
        ReadState m_ReadState;
    };
}
