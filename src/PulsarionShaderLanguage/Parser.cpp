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
        std::size_t start = PeekToken().Index;

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
                    return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, start, PeekToken().Index), children);
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

        return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, start, PeekToken().Index), children);
    }

    std::optional<SyntaxNode> Parser::ParseStatement()
    {
        std::size_t start = PeekToken().Index;
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

        return SyntaxNode(NodeDescriptor(NodeType::StatementNode, start, PeekToken().Index), children);
    }

    std::optional<SyntaxNode> Parser::ParseExpression()
	{

		std::vector<SyntaxNode> children;
		std::size_t start = PeekToken().Index;

        Token token = ReadToken();
        std::optional<SyntaxNode> result;
        while (IsValidExpressionToken(token))
        {
            Backtrack(1); // We backtrack one token, because we don't want to consume the first token of the expression.

            result = ParseLiteral();
            if (result.has_value())
                return result;

            result = ParseIdentifier();
            if (result.has_value())
                return result; // AN identifier is a valid expression, so we return it.

            (void)ReadToken(); // We consume the token, because we want to continue parsing.
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, PeekToken().Index, PeekToken().Index, token), {}));

            token = ReadToken();
        }

        switch (token.Type)
		{
		case TokenType::Semicolon:
            Backtrack(1); // We backtrack one token, because we don't want to consume the semicolon.
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, PeekToken().Index, PeekToken().Index, token), {}));
            break;
        default:
            // We backtrack one token and have the parent function deal with the token.
            Backtrack(1);
			break;
        }

        // For now we use a BinaryOperatorNode, but we will change it later to the proper node type.
        return SyntaxNode(NodeDescriptor(NodeType::BinaryOperatorNode, start, PeekToken().Index), children);
	}

    std::optional<SyntaxNode> Parser::ParseExpressionBacktrack()
    {
        std::size_t currentBacktrackIndex = m_CurrentTokenIndex;
        auto result = ParseExpression();
        m_CurrentTokenIndex = currentBacktrackIndex;
        return result;
    }


    std::optional<SyntaxNode> Parser::ParseIdentifier()
    {
        std::size_t start = PeekToken().Index;
        std::size_t peek = 0;
        Token next = PeekToken(peek);
        std::vector<SyntaxNode> children;

        if (next.Type == TokenType::ColonColon) // Optional '::' for namespaces
        {
            peek++;
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, next), {}));
            next = PeekToken(peek);
        }
        if (next.Type != TokenType::Identifier)
            return std::nullopt; // If the next token is not an identifier, we return nullopt.

        children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, next), {}));
        peek++;
        // Optional 0 - infinite amount of '::Identifier' for namespaces
        while (PeekToken(peek).Type == TokenType::ColonColon)
        {
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, PeekToken(peek)), {}));
            peek++;
            if (PeekToken(peek).Type != TokenType::Identifier)
                return std::nullopt; // If the next token is not an identifier, we return nullopt.
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, PeekToken(peek)), {}));
            peek++;
        }
        // Optional 0 - infinite amount of '.Identifier' for member access
        while (PeekToken(peek).Type == TokenType::Dot)
        {
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, PeekToken(peek)), {}));
            peek++;
            if (PeekToken(peek).Type != TokenType::Identifier)
                return std::nullopt; // If the next token is not an identifier, we return nullopt.
            children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, start + peek, start + peek, PeekToken(peek)), {}));
            peek++;
        }
        ConsumeToken(peek);

        return SyntaxNode(NodeDescriptor(NodeType::IdentifierNode, start, PeekToken().Index), children);
    }

    std::optional<SyntaxNode> Parser::ParseLiteral()
    {
        // Literal nodes are only one token, so we don't need to backtrack
        Token token = PeekToken();
        switch (token.Type)
        {
        case TokenType::Number:
        case TokenType::HexNumber:
        case TokenType::BinaryNumber:
        case TokenType::OctalNumber:
        case TokenType::Char:
        case TokenType::True:
        case TokenType::False:
            ConsumeToken(1);
            return SyntaxNode(NodeDescriptor(NodeType::LiteralNode, PeekToken().Index, PeekToken().Index, token), {});
        default:
            return std::nullopt;
        }
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

            switch (token.Type)
            {
            case TokenType::InvalidChar:
            case TokenType::InvalidNumber:
                m_Errors.push_back(ErrorInfo{ "Invalid Token", "", token.Line, token.Column });
                return Token(TokenType::EndOfFile, "", token.Line, token.Column, token.Index); // This will produce a false EOF error, but we do it like this for now.
            default:
                break;
            }

			m_TokensRead.push_back(token);
			m_CurrentTokenIndex++;
			return token;
		}


		Token token = m_TokensRead[m_CurrentTokenIndex];
		m_CurrentTokenIndex++;
		return token;
    }

    const Token& Parser::PeekToken(std::size_t n)
	{
        std::size_t requiredIndex = m_CurrentTokenIndex + n;
        if (requiredIndex >= m_TokensRead.size())
        {
            std::size_t tokensToRead = requiredIndex - m_TokensRead.size() + 1;
            // We set the current token to the last token, because we want to request additional tokens from the lexer.
            m_CurrentTokenIndex = m_TokensRead.size();
            for (std::size_t i = 0; i < tokensToRead; ++i)
            {
                // Casting the result of ReadToken to void to explicitly ignore it
                (void)ReadToken();
            }
            // Resetting the index pointer back to the original position
            m_CurrentTokenIndex = requiredIndex - n;
        }
        return m_TokensRead[requiredIndex];
	}

    Token Parser::PeekBackToken(std::size_t n)
    {
		if (m_CurrentTokenIndex < n)
        {
			m_Errors.push_back(ErrorInfo{ "PeekBackToken Error: Tried to peek back more tokens than read", "", 0, 0 });
            return {};
		}

        return m_TokensRead[m_CurrentTokenIndex - n];
    }

    void Parser::ConsumeToken(std::size_t n)
    {
        if (m_CurrentTokenIndex + n > m_TokensRead.size())
        {
            PULSARION_CORE_LOG_WARN("Parser::ConsumeToken: Tried to consume more tokens than read, this should not happen!");
            for (std::size_t i = 0; i < n; i++)
			{
                (void)ReadToken();
			}
            return; // We don't want to double increment the index.
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
