#include "Parser.hpp"
#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{

// =====================================================================================================================
// Constructor, Destructor, Boilerplate
// =====================================================================================================================

Parser::Parser(Lexer&& lexer)
    : m_LexerState(std::move(lexer)), m_ErrorState()
{
}

// =====================================================================================================================
// Parsing Functions
// =====================================================================================================================

Parser::ParseResult Parser::Parse()
{
    auto result = ParseScope();
    // It should return a result, since missing closing braces is a recoverable and non-fatal error that sets an error flag
    if (!result.Root || (result.Errors.size() > 0 && !result.WasRecovered))
    {
        // This should return early when there is an unrecoverable error or it doesn't return a result (which also means the error was unrecoverable)
        result.Root = std::nullopt;
        return result;
    }

    // We don't need to check if Root has a value, since it should return early if it doesn't

    result.ErrorFlags ^= 0x0000'0001; // Remove the missing closing brace error flag
    if (result.ErrorFlags == 0)
    {
        // There is only that one error, so we can clear the list
        result.Errors.clear();
        return result;
    }

    // If it did not return early, then it is not a valid result, we try to give detailed errors here
    result.ErrorFlags ^= 0x0000'0001; // We revert the change we made earlier

}

// =====================================================================================================================

// =====================================================================================================================
// LexerState Functions
// =====================================================================================================================


Token Parser::LexerState::Peek(std::size_t offset)
{
    if (CurrentTokenIndex + offset >= EndOfStreamIndex)
    {
        return Token(TokenType::EndOfFile);
    }

    // For the first token 0 + 0 = 0
    if (CurrentTokenIndex + offset >= TokensRead.size())
    {
        ReadTokens(CurrentTokenIndex + offset - TokensRead.size() + 1);
    }

    return TokensRead[CurrentTokenIndex + offset];
}

Token Parser::LexerState::Read()
{
    if (CurrentTokenIndex >= EndOfStreamIndex)
    {
        return Token(TokenType::EndOfFile);
    }

    Token token = Peek();
    CurrentTokenIndex++;
    return token;
}

void Parser::LexerState::ReadTokens(std::size_t count)
{
    Token token;
    for (std::size_t i = 0; i < count; i++)
    {
        token = Lexer.NextToken();
        switch (token.Type)
        {
        case TokenType::Comment:
        case TokenType::InlineComment:
            break; // Ignore comments
        default:
            TokensRead.push_back(token);
            break;
        }
    }
}

// =====================================================================================================================
}
