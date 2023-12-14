#pragma once

#include "Core.hpp"
#include "Lexer.hpp"
#include "AbstractSyntaxTree.hpp"

#include <optional>
#include <stack>

namespace Pulsarion::Shader
{
    struct ErrorInfo
    {
        std::string Message;
        std::string File;
        std::size_t Line;
        std::size_t Column;
    };

    class PULSARION_SHADER_LANGUAGE_API Parser
    {
    public:
        explicit Parser(Lexer&& lexer);
        ~Parser() = default;

        std::optional<SyntaxNode> Parse();
        [[nodiscard]] const std::vector<ErrorInfo>& GetErrors() const;

        static bool IsValidExpressionToken(const Token& token);
        static bool IsValidStatementToken(const Token& token);
        static bool IsStatementKeyword(const Token& token);
    private:
        std::optional<SyntaxNode> ParseScope(); // { ... }
        std::optional<SyntaxNode> ParseStatement();
        std::optional<SyntaxNode> ParseExpression();
        std::optional<SyntaxNode> ParseSubExpression();

        std::optional<SyntaxNode> ParseIdentifier();
        std::optional<SyntaxNode> ParseLiteral();
        std::optional<SyntaxNode> ParseKeyword();
        std::optional<SyntaxNode> ParseStatementKeyword();

        void ClearBacktrack();
        void Backtrack(std::size_t n);
        Token ReadToken();
        const Token& PeekToken(std::size_t n = 0);
        void ConsumeToken(std::size_t n = 1);

        std::size_t m_CurrentTokenIndex;
        std::vector<Token> m_TokensRead;
        std::stack<Token> m_ScopeStack;
        std::vector<ErrorInfo> m_Errors;
        Lexer m_Lexer;
    };
}
