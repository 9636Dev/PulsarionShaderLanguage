#include "Parser.hpp"
#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{
    // Represents parsing state of a parser function
    struct InternalParseState
    {
        std::vector<SyntaxNode> Nodes; // This is the child nodes of the current node
        SourceLocation Location; // This is the location of the current node

        InternalParseState(std::size_t start, std::size_t line, std::size_t column)
            : Nodes(), Location(start, line, column, 0)
        {
        }

        InternalParseState(Token token)
            : Nodes(), Location(token.Index, token.Line, token.Column, 0)
        {
        }

        SyntaxNode ToNode(NodeType type, std::size_t EndIndex, std::optional<Token> content = std::nullopt)
        {
            Location.Length = EndIndex - Location.Index;
            return SyntaxNode(type, Location, content, Nodes);
        }
    };

    Parser::Parser(Lexer&& lexer)
        : m_Lexer(std::move(lexer)), m_ReadState()
    {
    }

    Parser::~Parser()
    {
        // We don't need to do anything yet
    }

    std::optional<SyntaxNode> Parser::Parse()
    {
        auto result = ParseScope();
        if (!result)
            return std::nullopt; // There should already be an error in the error list

        // We check if we are at the end of the file, if not then there is an unexpected token '}'
        if (PeekToken().Type != TokenType::EndOfFile)
        {
            AddError(ErrorSeverity::Fatal, "Unexpected token '}' at the end of the file", GetLocationFor(PeekToken()));
            return std::nullopt;
        }

        return result;
    }

    /// <summary>
    /// Parses a scope. Doesn't consume the opening curly brace.
    /// Stops when it reaches a closing curly brace and consumes it or reaches EOF.
    std::optional<SyntaxNode> Parser::ParseScope(bool allowEOF)
    {
        // We assume that the last token was a left brace

        Token token = PeekToken();
        bool loopRunning = true;
        InternalParseState state(token);

        while (loopRunning)
        {
            switch (token.Type)
            {
            case TokenType::EndOfFile:
                if (allowEOF)
                {
                    loopRunning = false;
                    break;
                }
                // We don't break here, because we want to add an error
                AddError(ErrorSeverity::Fatal, "Unexpected end of file while trying to find scope ending", GetLocationFor(token));
                return std::nullopt;

            case TokenType::RightBrace:
                (void)ReadToken(); // Consume the right brace
                loopRunning = false;
                break;

            default:
                // TODO: Parse statements
                state.Nodes.push_back(SyntaxNode(ReadToken())); // This is temporary
            }

            token = PeekToken();
        }

        return state.ToNode(NodeType::Scope, PeekToken().Index);
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
            switch (tempToken.Type)
            {
            case TokenType::EndOfFile:
                return tempToken;
            case TokenType::Comment:
            case TokenType::InlineComment:
                continue;
            default:
                m_ReadState.ReadTokens.push_back(tempToken);
            }

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
