#include "Parser.hpp"

namespace Pulsarion::Shader
{
    Parser::Parser(Lexer&& lexer)
        : m_Lexer(std::move(lexer)), m_ScopeStack(), m_Errors()
    {
    }

    std::optional<SyntaxNode> Parser::Parse()
    {
        return ParseScope();
    }

    std::optional<SyntaxNode> Parser::ParseScope()
    {
        std::vector<SyntaxNode> children;
        std::size_t start = m_Lexer.GetPosition();

        // We parse each statement, they should automatically consume the tokens needed, we just have to check for scope changes after each statement.
        // If we encounter a scope increase, we throw an error, this is caused by missing symbols and therefore the closing brace is missing or invalid.
        Token token = Token(TokenType::Unknown, "", 0, 0); 
        do {
            token = m_Lexer.NextToken();
            if (token.Type == TokenType::EndOfFile)
                break; // We reached the end of the file, we can stop parsing.
            switch (token.Type)
            {
            case TokenType::LeftBrace: { 
                m_ScopeStack.push(token);
                auto result = ParseScope(); // The opening bracket is automatically consumed when we called 'NextToken' in the beginning of the loop. 
                if (result.has_value())
                    children.push_back(result.value());
                else
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
                break;
            }
            case TokenType::RightBrace: {
                if (m_ScopeStack.empty())
                {
                    m_Errors.push_back(ErrorInfo{ "Unexpected '}'", "", token.Line, token.Column });
                    return std::nullopt;
                }
                else
                {
                    if (m_ScopeStack.top().Type != TokenType::LeftBrace)
                    {
                        m_Errors.push_back(ErrorInfo{ "Unexpected '}' (Mismatched Closing Delimiter)", "", token.Line, token.Column });
                        return std::nullopt;
                    }
                    m_ScopeStack.pop();
                    return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, start, m_Lexer.GetPosition()), children);
                }
                break;
            }
            default:
                std::size_t currentScope = m_ScopeStack.size();
                auto result = ParseStatement(token);
                if (result.has_value())
                {
                    children.push_back(result.value());
                    if (currentScope != m_ScopeStack.size() && token.Type == TokenType::RightBrace) // The token should be changed by the ParseStatement function, so we can check it here.
                        break; // We just break out of the loop, because the ParseStatement function already consumed the closing brace.
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.

                    break;
                }
                else
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            }
        } while (!m_Lexer.IsEnd());
        
        if (m_Lexer.IsEnd() && !m_ScopeStack.empty())
        {
            m_Errors.push_back(ErrorInfo{ "Unexpected End of File (Missing Closing Delimiter)", "", token.Line, token.Column });
            return std::nullopt;
        }    

        return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, start, m_Lexer.GetPosition()), children);
    }

    std::optional<SyntaxNode> Parser::ParseStatement(Token& token)
    {
        std::vector<SyntaxNode> children;
        // We don't need a do-while loop here, because we already consumed the first token.
        while (token.Type != TokenType::Semicolon)
        {
            if (token.Type == TokenType::LeftBrace)
            {
                m_ScopeStack.push(token);
                auto result = ParseScope();
                if (result.has_value())
                    return result.value();
                else
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            }
            if (token.Type == TokenType::RightBrace)
            {
                if (m_ScopeStack.empty())
                {
                    m_Errors.push_back(ErrorInfo{ "Unexpected '}'", "", token.Line, token.Column });
                    return std::nullopt;
                }
                if (m_ScopeStack.top().Type != TokenType::LeftBrace)
                {
                    m_Errors.push_back(ErrorInfo{ "Unexpected '}' (Mismatched Closing Delimiter)", "", token.Line, token.Column });
                    return std::nullopt;
                }
                m_ScopeStack.pop();
                break; // We exit the loop so it returns normally.
            }
            else if (token.Type == TokenType::EndOfFile)
            {
                m_Errors.push_back(ErrorInfo{ "Unexpected End of File (Missing Semicolon)", "", token.Line, token.Column });
                return std::nullopt;
            }
            else
            {
                auto result = ParseExpression(token);
                if (result.has_value())
                    return result.value();
                else 
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            }

            token = m_Lexer.NextToken();
        }
    }
}
