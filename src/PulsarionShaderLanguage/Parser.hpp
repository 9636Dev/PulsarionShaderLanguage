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
        Parser(Lexer&& lexer);
        ~Parser() = default;

        std::optional<SyntaxNode> Parse();
        const std::vector<ErrorInfo>& GetErrors() const;
        
        static bool IsValidExpressionToken(const Token& token);
    private:
        std::optional<SyntaxNode> ParseScope(); // { ... }
        std::optional<SyntaxNode> ParseStatement();
        std::optional<SyntaxNode> ParseExpression(); 

        void ClearBacktrack();
        void Backtrack(std::size_t n);
        Token ReadToken();
        Token PeekToken(std::size_t n = 1);
        Token PeekBackToken(std::size_t n = 1);
        void ConsumeToken(std::size_t n = 1);

        std::size_t m_CurrentTokenIndex;
        std::vector<Token> m_TokensRead;
        std::stack<Token> m_ScopeStack;
        std::vector<ErrorInfo> m_Errors;
        Lexer m_Lexer;
    };
}
