#include "Parser.hpp"

#include <vector>

namespace Pulsarion::Shader
{
    Parser::Parser(Lexer&& lexer)
        : m_Lexer(std::move(lexer)), m_ScopeStack(), m_Errors(), m_TokensRead(), m_CurrentTokenIndex(0)
    {
    }

    const std::vector<ErrorInfo>& Parser::GetErrors() const
	{
		return m_Errors;
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
        bool breakLoop = false;
        Token token; 
        do {
            token = ReadToken();
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
                Backtrack(1);
                auto result = ParseStatement();
                if (result.has_value())
                {
                    children.push_back(result.value());
                    if (currentScope != m_ScopeStack.size() && PeekBackToken().Type == TokenType::RightBrace) // The token should be changed by the ParseStatement function, so we can check it here.
                        breakLoop = true; // We just break out of the loop, because the ParseStatement function already consumed the closing brace.
                    break;
                }
                else
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            }
        } while (!m_Lexer.IsEnd() && !breakLoop);
        
        if (m_Lexer.IsEnd() && !m_ScopeStack.empty())
        {
            m_Errors.push_back(ErrorInfo{ "Unexpected End of File (Missing Closing Delimiter)", "", token.Line, token.Column });
            return std::nullopt;
        }    

        return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, start, m_Lexer.GetPosition()), children);
    }

    std::optional<SyntaxNode> Parser::ParseStatement()
    {
        std::size_t start = m_Lexer.GetPosition();
        std::vector<SyntaxNode> children;
        Token token = ReadToken();
        bool breakLoop = false;
        while (token.Type != TokenType::Semicolon && token.Type != TokenType::EndOfFile)
        {
            switch (token.Type)
			{
                case TokenType::LeftBrace: {
                    m_ScopeStack.push(token);
                    auto result = ParseScope();
                    if (result.has_value())
                    {
                        children.push_back(result.value());
                        breakLoop = true;
                        break;
                    }
                    else
                        return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
                }
                case TokenType::RightBrace: {
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
                    breakLoop = true;
                    break; // We exit the loop so it returns normally.
                }
                default: {
                    // We backtrack one token, because we don't want to consume the first token of the expression.
                    Backtrack(1);
                    auto result = ParseExpression();
                    if (result.has_value())
                        children.push_back(result.value());
                    else 
                        return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
                    break;
                }
            }

            if (breakLoop)
				break;
            token = ReadToken();
        }

        switch (token.Type)
		{
        case TokenType::EndOfFile:
            m_Errors.push_back(ErrorInfo{ "Unexpected End of File (Missing Semicolon)", "", token.Line,     token.Column });
            return std::nullopt;
        default:
            break;
        }

        return SyntaxNode(NodeDescriptor(NodeType::StatementNode, start, m_Lexer.GetPosition()), children);
    }

    std::optional<SyntaxNode> Parser::ParseExpression()
	{
    
		std::vector<SyntaxNode> children;
		std::size_t start = m_Lexer.GetPosition();
        
        Token token = ReadToken();
        while (IsValidExpressionToken(token))
        {
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, m_Lexer.GetPosition(), m_Lexer.GetPosition(), token), {}));
            
            token = ReadToken();
        }

        switch (token.Type)
		{
		case TokenType::Semicolon:
            Backtrack(1); // We backtrack one token, because we don't want to consume the semicolon.
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, m_Lexer.GetPosition(), m_Lexer.GetPosition(), token), {}));
            break;
        default:
            // We backtrack one token and have the parent function deal with the token.
            Backtrack(1);
			break;
        }

        // For now we use a BinaryOperatorNode, but we will change it later to the proper node type.
        return SyntaxNode(NodeDescriptor(NodeType::BinaryOperatorNode, start, m_Lexer.GetPosition()), children);
	}

    void Parser::ClearBacktrack()
    {
        m_TokensRead.clear();
        m_CurrentTokenIndex = 0;
    }

    void Parser::Backtrack(std::size_t n)
	{
        if (m_CurrentTokenIndex < n)
        {
			m_Errors.push_back(ErrorInfo{ "Backtrack Error: Tried to backtrack more tokens than read", "", 0, 0 });
			m_CurrentTokenIndex = 0;
            return;
        }

        m_CurrentTokenIndex -= n;
	}

    Token Parser::ReadToken()
    {
		if (m_CurrentTokenIndex >= m_TokensRead.size())
		{
			Token token;
            do {
				token = m_Lexer.NextToken();
                // We dont push back comments, because they are not needed for the parser.
			} while (token.Type == TokenType::Comment); 
			m_TokensRead.push_back(token);
			m_CurrentTokenIndex++;
			return token;
		}
		else
		{
			Token token = m_TokensRead[m_CurrentTokenIndex];
			m_CurrentTokenIndex++;
			return token;
		}
    }

    Token Parser::PeekToken(std::size_t n)
	{
		if (m_CurrentTokenIndex + n >= m_TokensRead.size())
		{
            std::size_t toRead = (m_CurrentTokenIndex + n) - m_TokensRead.size();
            for (std::size_t i = 0; i < toRead; i++)
			{
				Token token = m_Lexer.NextToken();
				m_TokensRead.push_back(token);
			}
            return m_TokensRead[m_CurrentTokenIndex + n];
		}
		else
		{
			Token token = m_TokensRead[m_CurrentTokenIndex + n];
			return token;
		}
	}

    Token Parser::PeekBackToken(std::size_t n)
    {
		if (m_CurrentTokenIndex < n)
        {
			m_Errors.push_back(ErrorInfo{ "PeekBackToken Error: Tried to peek back more tokens than read", "", 0, 0 });
            return Token();
		}
        
        return m_TokensRead[m_CurrentTokenIndex - n];
    }

    void Parser::ConsumeToken(std::size_t n)
    {
        if (m_CurrentTokenIndex + n >= m_TokensRead.size())
        {
            PULSARION_CORE_LOG_WARN("Parser::ConsumeToken: Tried to consume more tokens than read, this should not happen!");
            for (std::size_t i = 0; i < n; i++)
			{
                (void)ReadToken();
			}
        }
		m_CurrentTokenIndex += n;
	}

    bool Parser::IsValidExpressionToken(const Token& token)
    {
        switch (token.Type)
        {
        case TokenType::EndOfFile:
        case TokenType::LeftBrace:
        case TokenType::RightBrace:
        case TokenType::Semicolon:
			return false;
        default:
            return true;
        }
    }
}
