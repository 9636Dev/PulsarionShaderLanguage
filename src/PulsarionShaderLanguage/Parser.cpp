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
        /// === Parse ===
        /// Scope (Parsed - it will parse the whole file, and return a ScopeNode)
        /// EOF (Not consumed)
        auto result = ParseScope();
        // We make sure it parsed the whole file, if it didn't we return nullopt.
        if (PeekToken().Type != TokenType::EndOfFile)
        {
            // Currently this is generic, but we will add more specific errors later.
            m_Errors.push_back(ErrorInfo{ "Unexpected Token", "", PeekToken().Line, PeekToken().Column });
            return std::nullopt;
        }
        return result;
    }

    std::optional<SyntaxNode> Parser::ParseScope()
    {
         // === ParseScope ===
        // { (Ignored - shouldn't be passed into ParseScope)
        // Statements ( Parsed - using recursive descent )
        // } (Consumed - it consumed this and returns)

        std::vector<SyntaxNode> children;

        // We parse each statement, they should automatically consume the tokens needed, we just have to check for scope changes after each statement.
        // If we encounter a scope increase, we throw an error, this is caused by missing symbols and therefore the closing brace is missing or invalid.
        bool breakLoop = false;
        Token token = PeekToken();
        std::size_t startIndex = token.Index;
        while (token.Type != TokenType::EndOfFile && !breakLoop)
        {
            switch (token.Type)
            {
                case TokenType::LeftBrace: {
                    ConsumeToken(1);
                    const auto result = ParseScope();
                    if (!result.has_value())
                        return std::nullopt;
                    children.push_back(result.value());
                    break;
                }
                case TokenType::RightBrace: {
                    ConsumeToken(1);
                    breakLoop = true;
                    break;
                }
                default: {
                    const auto result = ParseStatement();
                    if (!result.has_value())
                        return std::nullopt;
                    children.push_back(result.value());
                    break;
                }
            }

            // We peek the next token
            token = PeekToken();
        }

        // The token is the next token already, so we don't need to + 1 to the end, as it is exclusive.
        return SyntaxNode(NodeDescriptor(NodeType::ScopeNode, startIndex, token.Index), children);
    }

    std::optional<SyntaxNode> Parser::ParseStatement()
    {
        Token token = PeekToken();
        std::size_t startIndex = token.Index;
        std::vector<SyntaxNode> children;
        std::optional<SyntaxNode> result;

        while (IsValidStatementToken(token))
        {
            result = ParseStatementKeyword();
            if (result.has_value())
            {
                children.push_back(result.value());
                break;
            }

            result = ParseExpression();
            if (!result.has_value())
                return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            children.push_back(result.value());

            token = PeekToken();
        }

        // The proceeding token is the next token already
        // We have to check if the token is a semicolon.
        switch (token.Type)
        {
            case TokenType::Semicolon: {
                //// If it is a semicolon then we have to consume it and add a child node.
                ConsumeToken(1);
                children.push_back(SyntaxNode(NodeDescriptor(NodeType::TokenNode, token.Index, token.Index, token), {}));
            }

        }

        // The parent function will handle scopes and EOF.
        return SyntaxNode(NodeDescriptor(NodeType::StatementNode, startIndex, PeekToken().Index), children);
    }

    std::optional<SyntaxNode> Parser::ParseExpression()
	{
        bool breakLoop = false;
		std::vector<SyntaxNode> children;
        Token token = PeekToken();
		std::size_t start = token.Index;

        std::optional<SyntaxNode> result;
        while (IsValidExpressionToken(token) && !breakLoop)
        {
            // We don't have to account for scopes and semi-colons, because they are not valid ExpressionTokens.
            switch (token.Type)
            {
            case TokenType::LeftParenthesis:
                ConsumeToken(); // We have to manually consume the token;
                result = ParseExpression();
                if (result.has_value())
                {
                    children.push_back(result.value());
                    break;
                }
                return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.
            case TokenType::RightParenthesis:
                breakLoop = true; // We don't consume the token, because it is not part of this expression.
                break;
            default:
                result = ParseSubExpression();
                if (!result.has_value())
                    return std::nullopt; // It only returns nullopt when it fails, so we propagate the error.

                children.push_back(result.value()); // It should automatically consume the token.
                break;
            }

            token = PeekToken();
        }

        return SyntaxNode(NodeDescriptor(NodeType::ExpressionNode, start, token.Index), children);
	}

    std::optional<SyntaxNode> Parser::ParseSubExpression()
    {
        // This function is meant to only parse one construct of an expression, such as a function call, a binary operation, etc.
        Token token = PeekToken();
        // We don't have to account for parenthesis, semicolons, or scopes, because they are already handled by the ParseExpression function.

        // If the token is a statement keyword, we return nullopt, because it is not a valid expression token.
        if (IsStatementKeyword(token))
        {
            m_Errors.push_back(ErrorInfo{ "Unexpected Keyword", "", token.Line, token.Column });
            return std::nullopt;
        }

        // We try to parse a keyword, if it succeeds we return the result.
        auto result = ParseKeyword();
        if (result.has_value())
            return result;

        result = ParseLiteral();
        if (result.has_value())
            return result;

        result = ParseIdentifier();
        if (result.has_value())
            return result;

        // For a placeholder we just return the token.
        ConsumeToken(1);
        return SyntaxNode(NodeDescriptor(NodeType::TokenNode, token.Index, token.Index, token), {});
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

    std::optional<SyntaxNode> Parser::ParseKeyword()
    {
        Token token = ReadToken();
        switch (token.Type)
        {
            default: // We currently don't have any expression keywords
                Backtrack(1); // We backtrack one token, because we don't want to consume the token.
                return std::nullopt;
        }
    }

    std::optional<SyntaxNode> Parser::ParseStatementKeyword()
    {
        Token token = ReadToken();
        switch (token.Type)
        {
            case TokenType::If: {
                // We to try to read left parenthesis, if it fails we backtrack and return nullopt.
                if (ReadToken().Type != TokenType::LeftParenthesis)
                {
                    Backtrack(1);
                    m_Errors.push_back(ErrorInfo{ "Unexpected 'if' (Missing Left Parenthesis)", "", token.Line, token.Column });
                    return std::nullopt;
                }

                auto expression = ParseExpression(); // We try to parse the expression inside the parenthesis.
                if (!expression.has_value())
                {
                    m_Errors.push_back(ErrorInfo{ "Unexpected 'if' (Missing Condition)", "", token.Line, token.Column });
                    return std::nullopt;
                }

                // We to try to read right parenthesis, if it fails we backtrack and return nullopt.
                if (ReadToken().Type != TokenType::RightParenthesis)
                {
                    Backtrack(1);
                    m_Errors.push_back(ErrorInfo{ "Unexpected 'if' (Missing Right Parenthesis)", "", token.Line, token.Column });
                    return std::nullopt;
                }

                std::optional<SyntaxNode> result;
                if (PeekToken().Type == TokenType::LeftBrace)
                    result = ParseScope();
                else
                    result = ParseStatement();

                if (result.has_value())
                    return SyntaxNode(NodeDescriptor(NodeType::IfNode, token.Index, token.Index, token), { expression.value(), result.value() });

                m_Errors.push_back(ErrorInfo{ "Unexpected 'if' (Missing Statement)", "", token.Line, token.Column });
                return std::nullopt;
            }

            default:
                Backtrack(1); // We backtrack one token, because we don't want to consume the token.
                return std::nullopt;
        }
        /*        }
        case TokenType::Else: {
            auto result = ParseStatement();
            if (result.has_value())
                return SyntaxNode(NodeDescriptor(NodeType::ElseNode, token.Index, token.Index, token), { result.value() });

            m_Errors.push_back(ErrorInfo{ "Unexpected 'else' (Missing Statement)", "", token.Line, token.Column });
            return std::nullopt;

        }
        case TokenType::While:
            // TODO: Implement loop parsing, currently it only consumes the one token.
            return SyntaxNode(NodeDescriptor(NodeType::WhileNode, token.Index, token.Index, token), {});
        case TokenType::For:
            return SyntaxNode(NodeDescriptor(NodeType::ForNode, token.Index, token.Index, token), {});
        case TokenType::Return:
            return SyntaxNode(NodeDescriptor(NodeType::ReturnKeywordNode, token.Index, token.Index, token), {});
        case TokenType::Break:
            return SyntaxNode(NodeDescriptor(NodeType::BreakKeywordNode, token.Index, token.Index, token), {});
        case TokenType::Continue:
            return SyntaxNode(NodeDescriptor(NodeType::ContinueKeywordNode, token.Index, token.Index, token), {});
        case TokenType::Case:
            return SyntaxNode(NodeDescriptor(NodeType::CaseKeywordNode, token.Index, token.Index, token), {});
        */
    }


    // ================================
    // End of main parsing functions
    // ================================


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

    bool Parser::IsValidStatementToken(const Token& token)
    {
        switch (token.Type)
        {
            case TokenType::EndOfFile:
            case TokenType::Semicolon:
            case TokenType::LeftBrace:
            case TokenType::RightBrace:
                return false;
            default:
                return true;
        }
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

    bool Parser::IsStatementKeyword(const Token& token)
    {
        switch (token.Type)
        {
            case TokenType::If:
            case TokenType::Else:
            case TokenType::While:
            case TokenType::For:
            case TokenType::Return:
            case TokenType::Break:
            case TokenType::Continue:
            case TokenType::Case:
                return true;
            default:
                return false;
        }
    }
}
