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
        
    private:
        std::optional<SyntaxNode> ParseScope(); // { ... }
        std::optional<SyntaxNode> ParseStatement(Token& token); // We have to pass the first token, because we have to read to make sure the next token is actually a statement.
        std::optional<SyntaxNode> ParseExpression(Token& token); 

        std::stack<Token> m_ScopeStack;
        std::vector<ErrorInfo> m_Errors;
        Lexer m_Lexer;
    };
}
