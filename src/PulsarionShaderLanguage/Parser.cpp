#include "Parser.hpp"
#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{
    // =====================================================================================================================
    // Utility Structs and functions
    // =====================================================================================================================

    /// <summary>
    /// Keeps track of the starting location and other parsing information inside a parsing function
    /// </summary>
    struct Parser::InternalParseState
    {
        SourceLocation Location;
        std::vector<SyntaxNode> Children;
        std::list<ParserError> Errors;
        std::uint32_t ErrorFlags;

        InternalParseState(SourceLocation location, std::vector<SyntaxNode> children = {})
            : Location(location), Children(children), Errors(), ErrorFlags(0)
        {

        }

        InternalParseState(std::size_t line, std::size_t column, std::size_t index)
            : InternalParseState(SourceLocation(line, column, index, 0))
        {

        }

        InternalParseState(const Token& startToken)
            : InternalParseState(startToken.Location)
        {

        }

        void AddChild(SyntaxNode&& node)
        {
            Children.push_back(node);
        }

        SyntaxNode CreateNode(NodeType type, std::optional<Token> content = std::nullopt)
        {
            return SyntaxNode(type, Location, content, Children);
        }

        SourceLocation CreateLocation(const SourceLocation& location)
        {
            auto length = location.Index - location.Index;
            return SourceLocation(Location.Line, Location.Column, Location.Index, length);
        }

        ParseResult ToResult(SourceLocation endLocation, NodeType nodeType, std::optional<Token> nodeContent = std::nullopt)
        {
            Location = CreateLocation(endLocation);
            return ParseResult(CreateNode(nodeType, nodeContent), Errors, ErrorFlags);
        }
    };

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
        if (!result.Root.has_value())
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
        else if (result.ErrorFlags == 0x0000'0001)
        {
            // There was no closing brace error, so we have an extra closing brace
            result.Errors.push_front(ParserError(result.Root.value().Location, ParserError::ErrorSource::Scope, ErrorSeverity::Fatal, "Extra closing brace", 0x0000'0002));
            result.ErrorFlags = 0x0000'0002;
            result.ErrorFlags ^= 0x0000'0001; // Remove the missing closing brace error flag
            return result;
        }

        // If it did not return early, then it is not a valid result, we try to give detailed errors here
        result.ErrorFlags ^= 0x0000'0001; // We revert the change we made earlier

        // We just return the result normally, since it is not a valid result
        return result;
    }

    Parser::ParseResult Parser::ParseScope()
    {
        Token token = m_LexerState.Peek();
        InternalParseState state = InternalParseState(token);

        // We don't need to check for a starting brace
        do {
            switch (token.Type)
            {
            case TokenType::LeftBrace: {
                    // We consume the left brace
                    m_LexerState.Consume();

                    // We recursively parse the scope
                    auto result = ParseScope();
                    if (!result.Root)
                    {
                        // We return the result, since it is not a valid result
                        return result;
                    } 

                    // We add the scope to the children
                    state.AddChild(std::move(result.Root.value()));
                    // We add the errors to the list
                    state.Errors.splice(state.Errors.end(), result.Errors);
                    // We add the error flags
                    state.ErrorFlags |= result.ErrorFlags;
                    // We continue parsing
                    break;
                }
            case TokenType::RightBrace:
                // We consume the right brace
                m_LexerState.Consume();

                // We found the closing brace, so we return the result
                return state.ToResult(token.Location, NodeType::Scope);
            case TokenType::EndOfFile:
                // We should return an error with the missing closing brace flag set
                state.ErrorFlags |= 0x0000'0001;
                state.Errors.push_back(ParserError(state.CreateLocation(token.Location), ParserError::ErrorSource::Scope, ErrorSeverity::Fatal, "Missing closing brace", 0x0000'0001));
                return state.ToResult(token.Location, NodeType::Scope);
            default:
                // For now as a temporary solution we read the tokena and print it
                token = m_LexerState.Read();
                PULSARION_CORE_LOG_INFO("Token: {0}", token.ToString());
                break;
            }

            // We read the next token, since there will be changes after calling the other parsing functions
            token = m_LexerState.Peek();
        } while (true);
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

    void Parser::LexerState::Consume(std::size_t count)
    {
        if (CurrentTokenIndex + count >= EndOfStreamIndex)
        {
            return;
        }

        CurrentTokenIndex += count;
    }

    bool Parser::LexerState::Consume(TokenType type)
    {
        if (Peek().Type == type)
        {
            Consume();
            return true;
        }

        return false;
    }

    void Parser::LexerState::ReadTokens(std::size_t count)
    {
        Token token;
        for (std::size_t i = 0; i < count;)
        {
            token = Lexer.NextToken();
            switch (token.Type)
            {
            case TokenType::Comment:
            case TokenType::InlineComment:
                break; // Ignore comments
            default:
                TokensRead.push_back(token);
                i++;
                break;
            }
        }
    }

    // =====================================================================================================================
}
