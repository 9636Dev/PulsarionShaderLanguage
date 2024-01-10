#pragma once

#include "Core.hpp"
#include "Lexer.hpp"
#include "AbstractSyntaxTree.hpp"

#include <string>
#include <optional>
#include <list>

namespace Pulsarion::Shader
{
    /// <summary>
    /// Represents a parser error.
    /// </summary>
    struct ParserError
    {
        SourceLocation Location;
        ErrorSeverity Severity;
        std::string Message;

        ParserError(SourceLocation location, ErrorSeverity severity, std::string message)
            : Location(location), Severity(severity), Message(message)
        {
        }
    };

    class PULSARION_SHADER_LANGUAGE_API Parser
    {
    public:
        Parser(Lexer&& lexer);
        ~Parser();

        /// <summary>
        /// Parses the source code.
        /// </summary>
        std::optional<SyntaxNode> Parse();

    private:
        // ================== Internal Structs ==================
        struct ReadState
        {
            mutable std::vector<Token> ReadTokens;
            std::size_t CurrentIndex;

            ReadState() : ReadTokens(), CurrentIndex(0) {}
        };
        // ======================================================

        // ================= Read Functions =================
        Token PeekToken(std::size_t n = 0) const; // We look at the current token, which is the next token to be read.
        Token ReadToken(); // We read the current token.
        bool ConsumeToken(TokenType type); // We consume the current token if it is of the given type, doesn't consume the token if it fails.
        void Backtrack(std::size_t n = 1); // We go back n tokens.
        // ==================================================

        // ================== Error Handling =================
        SourceLocation GetLocationFor(const Token& token) const;
        void AddError(ErrorSeverity severity, std::string message, SourceLocation loc);
        ParserError PopError();
        // ===================================================

        // ================== Parsing Functions ==================
        std::optional<SyntaxNode> ParseScope(bool allowEOF = false);

        mutable Lexer m_Lexer; // We will take ownership of the lexer.
        ReadState m_ReadState;
        std::list<ParserError> m_Errors; // We use a linked list, because we will be adding and removing errors a lot. In the future we might handle errors asynchronously, so we need to be able to add and remove errors from the list without invalidating iterators.
    };
}
