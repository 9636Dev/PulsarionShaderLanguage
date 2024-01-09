#include "Parser.hpp"

namespace Pulsarion::Shader
{
    Parser::Parser(Lexer&& lexer)
        : m_Lexer(std::move(lexer)), m_ReadState()
    {

    }

    Parser::~Parser()
    {
        // We don't need to do anything yet
    }

    Token Parser::PeekToken(std::size_t n) const
    {
        Token tempToken;
        while (m_ReadState.ReadTokens.size() <= n + m_ReadState.CurrentIndex)
        {
            tempToken = m_Lexer.NextToken();
            if (tempToken.Type == TokenType::EndOfFile)
                break;
            m_ReadState.ReadTokens.push_back(tempToken);
        }

        // We return tempToken regardless of whether it's EOF or not, since it will always return the correct token
        return tempToken;
    }

    Token Parser::ReadToken()
    {
        Token token = PeekToken();

        m_ReadState.CurrentIndex++;

        return token;
    }

    bool Parser::ConsumeToken(TokenType type)
    {
        Token token = PeekToken();

        if (token.Type != type)
            return false;

        m_ReadState.CurrentIndex++;
        return true;
    }

    void Parser::Backtrack(std::size_t n)
    {
        if (n > m_ReadState.CurrentIndex)
        {
            n = 0;
            // TODO: We should probably add an error to the parser here
        }
        m_ReadState.CurrentIndex -= n;
    }
}
