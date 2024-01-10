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
        // We use a 1 based index, because we are using an unsigned integer, we can't go below 0
        std::size_t index = m_ReadState.CurrentIndex + n;
        Token tempToken;

        // We read until the size it bigger than the index
        while (m_ReadState.ReadTokens.size() <= index)
        {
            tempToken = m_Lexer.NextToken();
            if (tempToken.Type == TokenType::EndOfFile)
                return tempToken;
            m_ReadState.ReadTokens.push_back(tempToken);
        }

        if (index >= m_ReadState.ReadTokens.size())
        {
            PULSARION_CORE_LOG_WARN("Index out of bounds: {} >= {}", index, m_ReadState.ReadTokens.size());
            return Token(TokenType::EndOfFile, "", 0, 0, 0); // We don't care about the line or column of the EOF token, as it is never used
        }

        return m_ReadState.ReadTokens[index];
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

    SourceLocation Parser::GetLocationFor(const Token& token) const
    {
        return SourceLocation { token.Line, token.Column, token.Index, token.Value.size() };
    }

    void Parser::AddError(ErrorSeverity severity, std::string message, SourceLocation loc)
    {
        m_Errors.push_back(ParserError(loc, severity, message));
    }

    ParserError Parser::PopError()
    {
        ParserError error = m_Errors.front();
        m_Errors.pop_front();
        return error;
    }
}
