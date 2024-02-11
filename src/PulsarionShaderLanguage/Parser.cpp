#include "AbstractSyntaxTree.hpp"
#include "PulsarionShaderLanguage/Token.hpp"
#include "Parser.hpp"

namespace Pulsarion::Shader
{
    // =====================================================================================================================
    // Utility Structs and functions
    // =====================================================================================================================


    /// <summary>
    /// A helper class for statement parsing.
    /// </summary>
    struct StatementHelper
    {
        struct TokenInfo
        {
            bool IsEndOfStatement;
            bool ShouldConsume;

            explicit TokenInfo(const bool isEndOfStatement = false, const bool shouldConsume = false)
                : IsEndOfStatement(isEndOfStatement), ShouldConsume(shouldConsume)
            {

            }
        };

        static TokenInfo GetTokenInfo(const TokenType type)
        {
            switch (type)
            {
            case TokenType::Semicolon:
                return TokenInfo(true, true); // We can consume the semicolon, since it has no meaning
            case TokenType::RightBrace:
            case TokenType::EndOfFile:
                return TokenInfo(true, false);
            default:
                return TokenInfo(false, false);
            }
        }
    };

    struct OperatorInfo
    {
        enum class Type
        {
            Numeric,
            Comparison,
            Logical,
            Ternary,
            Invalid
        };

        std::uint16_t Precedence;
        Type type;

        OperatorInfo(const std::uint16_t precedence, const Type type)
            : Precedence(precedence), type(type)
        {
        }
    };


    OperatorInfo GetOperatorInfo(TokenType type) {
        switch (type) {
        case TokenType::Asterisk:
        case TokenType::Slash:
        case TokenType::Percent:
            return OperatorInfo(11, OperatorInfo::Type::Numeric);
        case TokenType::Plus:
        case TokenType::Minus:
            return OperatorInfo(10, OperatorInfo::Type::Numeric);
        case TokenType::LeftShift:
        case TokenType::RightShift:
            return OperatorInfo(9, OperatorInfo::Type::Numeric);
        case TokenType::LessThan:
        case TokenType::LessThanEqual:
        case TokenType::GreaterThan:
        case TokenType::GreaterThanEqual:
            return OperatorInfo(8, OperatorInfo::Type::Comparison);
        case TokenType::EqualEqual:
        case TokenType::NotEqual:
            return OperatorInfo(7, OperatorInfo::Type::Comparison);
        case TokenType::Ampersand:
            return OperatorInfo(6, OperatorInfo::Type::Logical);
        case TokenType::Caret:
            return OperatorInfo(5, OperatorInfo::Type::Logical);
        case TokenType::Pipe:
            return OperatorInfo(4, OperatorInfo::Type::Logical);
        case TokenType::LogicalAnd:
            return OperatorInfo(3, OperatorInfo::Type::Logical);
        case TokenType::LogicalOr:
            return OperatorInfo(2, OperatorInfo::Type::Logical);
        case TokenType::Question:
            return OperatorInfo(1, OperatorInfo::Type::Ternary);
        default:
            return OperatorInfo(0, OperatorInfo::Type::Invalid);
        }
    }

    /// <summary>
    /// Keeps track of the starting location and other parsing information inside a parsing function
    /// </summary>
    struct Parser::InternalParseState
    {
        SourceLocation Location;
        std::vector<SyntaxNode> Children;
        std::list<Parsing::Error> Errors;
        std::list<Parsing::Error> Warnings;

        explicit InternalParseState(const SourceLocation &location, std::vector<SyntaxNode>&& children = {})
            : Location(location), Children(children)
        {

        }

        InternalParseState(const std::size_t line, const std::size_t column, const std::size_t index)
            : InternalParseState(SourceLocation(line, column, index, 0))
        {

        }

        explicit InternalParseState(const Token& startToken)
            : InternalParseState(startToken.Location)
        {

        }

        void AddChild(SyntaxNode&& node)
        {
            Children.push_back(node);
        }

        void AddChild(Parsing::Result&& result)
        {
            PULSARION_ASSERT(result.Success(), "The result must be successful to add it as a child!");
            PULSARION_ASSERT(result.Root.has_value(), "The result must have a root to add it as a child!");
            AddChild(std::move(result.Root.value()));
            AddErrors(std::move(result));
        }

        void AddErrors(Parsing::Result&& result)
        {
            Errors.splice(Errors.end(), result.Errors);
            Warnings.splice(Warnings.end(), result.Warnings);
        }

        [[nodiscard]] SyntaxNode CreateNode(const NodeType type, std::optional<Token>&& content = std::nullopt) const
        {
            return SyntaxNode(type, Location, content, Children);
        }

        [[nodiscard]] SourceLocation ToLocation(const SourceLocation& endLocation) const
        {
            const auto length = endLocation.Index - Location.Index;
            return SourceLocation(Location.Line, Location.Column, Location.Index, length);
        }

        Parsing::Result ToResult(const SourceLocation &endLocation, const NodeType nodeType, std::optional<Token>&& nodeContent = std::nullopt)
        {
            Location = ToLocation(endLocation);
            return Parsing::Result(CreateNode(nodeType, std::move(nodeContent)), std::move(Errors), std::move(Warnings));
        }

        Parsing::Result ToErrorResult(const SourceLocation &endLocation)
        {
            Location = ToLocation(endLocation);
            return Parsing::Result(std::nullopt, std::move(Errors), std::move(Warnings));
        }
    };

    struct Parser::BacktrackState
    {
        LexerState& LexerState;
        std::size_t BacktrackTo;
        bool ShouldBacktrack;

        BacktrackState(class LexerState& LexerState, const std::size_t backtrackTo)
            : LexerState(LexerState), BacktrackTo(backtrackTo), ShouldBacktrack(true)
        {

        }

        // Updates the backtrack point to the current position
        void UpdateBacktrackPoint()
        {
            BacktrackTo = LexerState.CurrentTokenIndex;
        }

        void KeepChanges()
        {
            ShouldBacktrack = false;
        }

        void DiscardChanges()
        {
            ShouldBacktrack = true;
        }

        void UpdateImmediately() const
        {
            if (!ShouldBacktrack)
                return;

            LexerState.BacktrackTo(BacktrackTo);
        }

        void Forcebacktrack() const
        {
            LexerState.BacktrackTo(BacktrackTo);
        }

        ~BacktrackState()
        {
            UpdateImmediately();
        }
    };

    // =====================================================================================================================
    // Constructor, Destructor, Boilerplate
    // =====================================================================================================================

    Parser::Parser(Lexer&& lexer)
        : m_LexerState(std::move(lexer))
    {
    }

    // =====================================================================================================================
    // Parsing Functions
    // =====================================================================================================================

    Parsing::Result Parser::Parse()
    {
        auto result = ParseScope();

        if (!result.Root.has_value())
            return result;

        if (result.Errors.empty())
        {
            result.Errors.emplace_back(result.Root->Location, Parsing::ErrorSource::Scope, ErrorSeverity::Fatal,
                                       Parsing::ErrorType::UnexpectedExtraClosingBrace);
            return result;
        }
        const auto it = std::find_if(
                            result.Errors.begin(), result.Errors.end(), [](const Parsing::Error &error)
                            { return error.Type == Parsing::ErrorType::UnexpectedEndOfFileWhenFindingClosingBrace && error.NestingLevel == 0; });
        if (it != result.Errors.end())
        {
            // We remove the error, because we don't expect a closing brace
            result.Errors.erase(it);
        }

        // It has fails since there are other errors, but for now we just return the result
        if (!result.Errors.empty())
            return result;

        return result;
    }

    Parsing::Result Parser::ParseScope()
    {
        Token token = m_LexerState.Peek();
        InternalParseState state(token);

        // We don't need to check for a starting brace
        do {
            switch (token.Type)
            {
            case TokenType::LeftBrace: {
                    // We consume the left brace
                    m_LexerState.Consume();

                    // We recursively parse the scope
                    auto result = ParseScope();
                    result.Nest();

                    if (!result.Success())
                        return result; // We return the result, since it is not a valid result

                    // We add the scope to the children
                    state.AddChild(std::move(result));
                    break;
                }
            case TokenType::RightBrace:
                // We consume the right brace
                m_LexerState.Consume();

                // We found the closing brace, so we return the result
                return state.ToResult(token.Location, NodeType::Scope);
            case TokenType::EndOfFile:
                // We should return an error with the missing closing brace flag set
                state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Scope,
                    ErrorSeverity::Fatal, Parsing::ErrorType::UnexpectedEndOfFileWhenFindingClosingBrace);
                return state.ToResult(token.Location, NodeType::Scope);
            default: {
                    auto result = ParseStatement();
                    result.Nest();

                    if (result.Success())
                    {
                        state.AddChild(std::move(result));
                        break;
                    }

                    // There is something wrong, so we can just return the result
                    state.AddErrors(std::move(result));
                    return state.ToErrorResult(m_LexerState.Peek().Location);
                }
            }

            // We read the next token, since there will be changes after calling the other parsing functions
            token = m_LexerState.Peek();
        } while (true);
    }

    Parsing::Result Parser::ParseStatement()
    {
        InternalParseState state(m_LexerState.Peek());

        BacktrackState backtrackState = m_LexerState.Snapshot();
        backtrackState.KeepChanges(); // By default we keep changes

        auto res = ParseKeywords();
        if (ShouldReturnStatement(res.first, res.second, state, backtrackState))
            return res.first;

        Parsing::Result result = ParseAssignment();
        if (ShouldReturnStatement(result, true, state, backtrackState))
            return result;

        result = ParseDeclarations();
        if (ShouldReturnStatement(result, true, state, backtrackState))
            return result;

        result = ParseStruct();
        if (ShouldReturnStatement(result, false, state, backtrackState))
            return result;

        result = ParseFunction();
        if (ShouldReturnStatement(result, false, state, backtrackState))
            return result;

        result = ParseExpression();
        if (ShouldReturnStatement(result, true, state, backtrackState))
        {
            state.Errors.clear();
            state.Warnings.clear();
            state.AddChild(std::move(result));
            return state.ToResult(m_LexerState.Peek().Location, NodeType::Statement);
        }

        return state.ToErrorResult(m_LexerState.Peek().Location);
    }

    bool Parser::ShouldReturnStatement(Parsing::Result &result, bool requireEOS, InternalParseState& state, BacktrackState& backtrackState)
    {
        result.Nest();
        if (!result.Success())
        {
            state.AddErrors(std::move(result));
            return false;
        }
        state.AddErrors(std::move(result));

        const auto token = m_LexerState.Peek();
        if (const auto tokenInfo = StatementHelper::GetTokenInfo(token.Type);
            requireEOS && !tokenInfo.IsEndOfStatement)
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Statement,
                ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedEndOfStatement);

            backtrackState.Forcebacktrack();
            return false;
        }

        auto tokenInfo = StatementHelper::GetTokenInfo(m_LexerState.Peek().Type);
        while (tokenInfo.ShouldConsume && tokenInfo.IsEndOfStatement) // We consume until we reach the start of the next line
        {
            m_LexerState.Consume();
            tokenInfo = StatementHelper::GetTokenInfo(m_LexerState.Peek().Type);
        }

        return true;
    }

    std::pair<Parsing::Result, bool> Parser::ParseKeywords()
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Read();
        InternalParseState state(token);

        switch (token.Type)
        {
        case TokenType::Return: {
            auto expression = ParseExpression();
            expression.Nest();
            if (!expression.Success())
            {
                if (m_LexerState.Peek().Type == TokenType::Semicolon)
                {
                    backtrackState.KeepChanges();
                    return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::ReturnNoValue), true);
                }

                state.AddErrors(std::move(expression));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordReturn,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpressionForReturnStatement);
                return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
            }
            state.AddChild(std::move(expression));
            backtrackState.KeepChanges();
            return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::ReturnExpression), true);
        }
        case TokenType::If: {
            if (!m_LexerState.Consume(TokenType::LeftParenthesis))
            {
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordIf,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedLeftParenthesisForIfCondition);
                return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
            }

            auto expression = ParseExpression();
            expression.Nest();

            if (!expression.Success())
            {
                state.AddErrors(std::move(expression));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordIf,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpression);
                return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
            }

            state.AddChild(std::move(expression));

            if (!m_LexerState.Consume(TokenType::RightParenthesis))
            {
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordIf,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedRightParenthesisForIfCondition);
                return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
            }

            if (m_LexerState.Consume(TokenType::LeftBrace))
            {
                // We have a scope
                auto scope = ParseScope();
                scope.Nest();
                if (!scope.Success())
                {
                    state.AddErrors(std::move(scope));
                    state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordIf,
                        ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedScopeForIfBody);
                    return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
                }

                state.AddChild(std::move(scope));
                backtrackState.KeepChanges();
                return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::If), false);
            }

            // We don't have a scope, so we parse a statement
            auto statement = ParseStatement();

            if (!statement.Success())
            {
                state.AddErrors(std::move(statement));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::KeywordIf,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedStatementForIfBody);
                return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), false);
            }

            state.AddChild(std::move(statement));
            backtrackState.KeepChanges();
            return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::If), false);
        }
        case TokenType::Break:
            backtrackState.KeepChanges();
            return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::Break), true);
        case TokenType::Continue:
            backtrackState.KeepChanges();
            return std::make_pair(state.ToResult(m_LexerState.Peek().Location, NodeType::Continue), true);
        default:
            backtrackState.DiscardChanges();
            break;
        }

        state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Keyword, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedKeyword);
        return std::make_pair(state.ToErrorResult(m_LexerState.Peek().Location), true);
    }



    Parsing::Result Parser::ParseExpression()
    {
        auto result = ParseExpression(0);
        // If it is empty we add an error, since it means there is no expression
        if (!result.Root.has_value())
        {
            InternalParseState state(m_LexerState.Peek());
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpression);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }
        return Parsing::Result(std::move(result.Root), std::move(result.Errors), std::move(result.Warnings));
    }

    Parsing::Result Parser::ParseDeclarations()
    {
        auto backtrackState = m_LexerState.Snapshot();
        const auto token = m_LexerState.Peek();
        InternalParseState state(token);
        auto type = NodeType::FunctionDeclaration;
        if (token.Type == TokenType::Struct)
        {
            type = NodeType::StructDeclaration;
            m_LexerState.Consume();
        }
        else
        {
            auto identifier = ParseIdentifier();
            identifier.Nest();
            if (!identifier.Success())
            {
                state.AddErrors(std::move(identifier));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Declaration,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForReturnType);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
            state.AddErrors(std::move(identifier));

             // The first value of a function declaration is the return type
            state.AddChild(std::move(identifier));
        }

        // Now we parse the name of the function or struct, we don't support templates yet
        auto identifier = ParseIdentifier();
        identifier.Nest();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Declaration,
                ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForReturnType);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }
        state.AddErrors(std::move(identifier));

        state.AddChild(std::move(identifier));

        if (type == NodeType::StructDeclaration)
        {
            // It should end, since structs don't have arguments;
            backtrackState.KeepChanges();
            return state.ToResult(m_LexerState.Peek().Location, NodeType::StructDeclaration);
        }

        // We have a function declaration, so we have to parse the arguments. We only care about the types, since the names are not important (Unless in the future we make a language server)
        if (!m_LexerState.Consume(TokenType::LeftParenthesis))
        {
            SourceLocation loc = m_LexerState.Peek().Location;
            state.Errors.emplace_back(loc, Parsing::ErrorSource::Declaration, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedOpeningParenthesisForFunctionArguments);
            return state.ToErrorResult(loc);
        }

        auto argumentList = SyntaxNode(NodeType::FunctionArgumentList, token.Location);
        do
        {
            auto argumentType = ParseIdentifier(true, false);
            argumentType.Nest();
            if (!argumentType.Success())
            {
                state.AddErrors(std::move(argumentType));
                state.Errors.emplace_back(argumentType.Root->Location, Parsing::ErrorSource::Declaration,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForArgumentType);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
            state.AddErrors(std::move(argumentType));

            // There is optional argumentName, so we try to parse it, but we don't care if it fails
            auto argumentName = ParseIdentifier(false, false);

            argumentList.Children.emplace_back(std::move(argumentType.Root.value()));
        } while (m_LexerState.Consume(TokenType::Comma));

        if (!m_LexerState.Consume(TokenType::RightParenthesis))
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Declaration,
                ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedClosingParenthesisForFunctionArguments);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        state.AddChild(std::move(argumentList));

        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::FunctionDeclaration);
    }

    Parsing::Result Parser::ParseIdentifier(bool allowNamespace, bool allowTrailing)
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        SyntaxNode namespaceNode(NodeType::Namespace, token.Location);

        Token nodeRoot;
        if (allowNamespace)
        {
            // Optional global namespace
            if (m_LexerState.Consume(TokenType::ColonColon))
            {
                // Global namespace
                namespaceNode.Children.emplace_back(NodeType::Token, token.Location, token);
            }

            token = m_LexerState.Read();

            if (token.Type != TokenType::Identifier)
            {
                state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Identifier,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForIdentifier);
                return state.ToErrorResult(token.Location);
            }
            namespaceNode.Children.emplace_back(NodeType::Token, token.Location, token);

            // If the consume fails it doesn't update the token, so its fine
            while (m_LexerState.Consume(TokenType::ColonColon, token))
            {
                namespaceNode.Children.emplace_back(NodeType::Token, token.Location, token);
                token = m_LexerState.Read();
                if (token.Type != TokenType::Identifier)
                {
                    state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Identifier,
                        ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForIdentifier);
                    return state.ToErrorResult(token.Location);
                }
                namespaceNode.Children.emplace_back(NodeType::Token, token.Location, token);
            }
            nodeRoot = token;
            namespaceNode.Children.pop_back(); // Remove the last identifier
        }
        else
        {
            if (token.Type != TokenType::Identifier)
            {
                state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Identifier,
                    ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForIdentifier);
                return state.ToErrorResult(token.Location);
            }
            nodeRoot = m_LexerState.Read();
        }


        SyntaxNode trailingNode(NodeType::Identifier, token.Location);
        // Either :: or . can be used to access members, but they don't mix

        if (allowTrailing)
        {
            while (true)
            {
                if (!m_LexerState.Consume(TokenType::Dot, token))
                    break;
                trailingNode.Children.emplace_back(NodeType::Token, token.Location, token);
                token = m_LexerState.Read();
                if (token.Type != TokenType::Identifier)
                {
                    state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Identifier,
                        ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForIdentifier);
                    return state.ToErrorResult(token.Location);
                }
                trailingNode.Children.emplace_back(NodeType::Token, token.Location, token);
            }
        }
        else if (m_LexerState.Consume(TokenType::Dot, token))
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Identifier,
                ErrorSeverity::Fatal, Parsing::ErrorType::UnexpectedDotInNonTrailingIdentifier);
            return state.ToErrorResult(token.Location);
        }

        if (!namespaceNode.Children.empty())
            state.AddChild(std::move(namespaceNode));
        if (!trailingNode.Children.empty())
            state.AddChild(std::move(trailingNode));

        backtrackState.KeepChanges();
        return state.ToResult(token.Location, NodeType::Identifier, nodeRoot);
    }

    Parsing::Result Parser::ParseStruct()
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        // We have a struct keyword
        if (!m_LexerState.Consume(TokenType::Struct))
        {
            state.Errors.emplace_back(token.Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedStructKeyword);
            return state.ToErrorResult(token.Location);
        }

        // We have a struct keyword, so we can parse the name
        auto identifier = ParseIdentifier(true, false); // We allow namespaces, but not trailing
        identifier.Nest();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForStructName);
            return state.ToErrorResult(token.Location);
        }
        state.AddErrors(std::move(identifier));

        if (!m_LexerState.Consume(TokenType::LeftBrace))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedOpeningBraceForFunctionBody);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        auto scope = ParseScope();
        scope.Nest();
        if (!scope.Success())
        {
            state.AddErrors(std::move(scope));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedScopeForStructDefinition);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }
        state.AddErrors(std::move(scope));

        PULSARION_ASSERT(scope.Root.has_value(), "The scope should have a root value!");
        for (auto& child : scope.Root.value().Children)
        {
            switch (child.Type)
            {
            case NodeType::VariableDeclaration:
                // We don't support definitions yet
                child.Type = NodeType::StructMemberVariable;
                break;
            case NodeType::VariableDefinition:
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::VariableDefinitionNotAllowedInStruct);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            case NodeType::FunctionDefinition:
                child.Type = NodeType::StructMemberFunction;
                break;
            case NodeType::FunctionDeclaration:
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::FunctionDeclarationNotAllowedInStruct);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            case NodeType::StructDeclaration:
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::StructDeclarationNotAllowedInStruct);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            case NodeType::StructDefinition:
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::StructDefinitionNotAllowedInStruct);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            default:
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Struct, ErrorSeverity::Fatal, Parsing::ErrorType::UnexpectedNodeTypeInStruct);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
        }

        // We have a valid struct, so we can return the result
        PULSARION_ASSERT(identifier.Root.has_value(), "The identifier should have a root value!");


        state.AddChild(std::move(identifier.Root.value()));
        state.AddChild(std::move(scope.Root.value()));
        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::StructDefinition);
    }

    Parsing::Result Parser::ParseFunction()
    {
        // We have an identifier which is the return type
        // We have an identifier which is the function name
        // We have an argument list
        // We have a scope

        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        // We have an identifier which is the return type
        auto returnType = ParseIdentifier(true, false);
        returnType.Nest();
        if (!returnType.Success())
        {
            state.AddErrors(std::move(returnType));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForReturnType);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        // We have an identifier which is the function name
        auto functionName = ParseIdentifier(true, false);
        functionName.Nest();
        if (!functionName.Success())
        {
            state.AddErrors(std::move(functionName));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForFunctionName);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        if (!m_LexerState.Consume(TokenType::LeftParenthesis))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedOpeningParenthesisForFunctionArguments);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        // We have a valid function declaration, we have a list of 2 identifiers separated by commas
        SyntaxNode argumentList(NodeType::FunctionArgumentList, token.Location);
        do {
            auto argumentType = ParseIdentifier(true, false);
            argumentType.Nest();
            if (!argumentType.Success())
            {
                state.AddErrors(std::move(argumentType));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForArgumentType);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
            state.AddErrors(std::move(argumentType));

            // The name can only be one identifier token
            auto argumentName = ParseIdentifier(false, false);
            argumentName.Nest();
            if (!argumentName.Success())
            {
                state.AddErrors(std::move(argumentName));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForArgumentName);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
            state.AddErrors(std::move(argumentName));

            SyntaxNode argument(NodeType::FunctionArgument, token.Location);
            argument.Children.emplace_back(std::move(argumentType.Root.value()));
            argument.Children.emplace_back(std::move(argumentName.Root.value()));

            argumentList.Children.emplace_back(std::move(argument));
        } while (m_LexerState.Consume(TokenType::Comma));

        if (!m_LexerState.Consume(TokenType::RightParenthesis))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedClosingParenthesisForFunctionArguments);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        // We don't support inline functions, so we expect a scope
        if (!m_LexerState.Consume(TokenType::LeftBrace))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedOpeningBraceForFunctionBody);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        auto scope = ParseScope();
        scope.Nest();
        if (!scope.Success())
        {
            state.AddErrors(std::move(scope));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Function, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedScopeForFunctionBody);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        state.AddChild(std::move(returnType));
        state.AddChild(std::move(functionName));
        state.AddChild(std::move(argumentList));
        state.AddChild(std::move(scope));

        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::FunctionDefinition);
    }

    Parsing::Result Parser::ParseAssignment()
    {
        InternalParseState state(m_LexerState.Peek());
        auto backtrackState = m_LexerState.Snapshot();

        // First we parse declaration, since it is a subset of assignment
        auto result = ParseDeclaration();
        if (result.Success())
        {
            backtrackState.KeepChanges();
            return result;
        }
        backtrackState.UpdateImmediately();

        // We don't have a declaration, so we try to parse an assignment
        auto identifier = ParseIdentifier();
        identifier.Nest();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForAssignment);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        NodeType type = NodeType::Token; // This is a placeholder
        Token token = m_LexerState.Read();
        switch (token.Type)
        {
        case TokenType::PlusEqual:
            type = NodeType::AssignmentAdd;
            break;
        case TokenType::MinusEqual:
            type = NodeType::AssignmentSubtract;
            break;
        case TokenType::AsteriskEqual:
            type = NodeType::AssignmentMultiply;
            break;
        case TokenType::SlashEqual:
            type = NodeType::AssignmentDivide;
            break;
        case TokenType::PercentEqual:
            type = NodeType::AssignmentModulo;
            break;
        case TokenType::LeftShiftEqual:
            type = NodeType::AssignmentBitwiseLeftShift;
            break;
        case TokenType::RightShiftEqual:
            type = NodeType::AssignmentBitwiseRightShift;
            break;
        case TokenType::AmpersandEqual:
            type = NodeType::AssignmentBitwiseAnd;
            break;
        case TokenType::CaretEqual:
            type = NodeType::AssignmentBitwiseXor;
            break;
        case TokenType::PipeEqual:
            type = NodeType::AssignmentBitwiseOr;
            break;
        default:
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedAssignmentOperator);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        auto expression = ParseExpression();
        expression.Nest();
        if (!expression.Success())
        {
            state.AddErrors(std::move(expression));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpressionForAssignment);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        state.AddChild(std::move(identifier));
        state.AddChild(std::move(expression));
        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, type, token);
    }

    Parsing::Result Parser::ParseDeclaration()
    {
        auto token = m_LexerState.Peek();
        InternalParseState state(token);
        auto backtrackState = m_LexerState.Snapshot();

        // There could be annotations, so we try to parse them first
        auto annotation = ParseAnnotation();
        annotation.Nest();
        if (annotation.Success())
            state.AddChild(std::move(annotation));

        // Auto will require type deduction
        if (!m_LexerState.Consume(TokenType::Auto))
        {
            auto result = ParseIdentifier();
            result.Nest();
            if (!result.Success())
            {
                state.AddErrors(std::move(result));
                state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForVariableDeclaration);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }
            state.AddChild(std::move(result));
        }
        else
            state.AddChild(SyntaxNode(NodeType::KeywordAuto, token.Location));

        // We have a valid type for the assignment.

        // There should be another identifier, we current don't support unpackings auto [a, b] = ...
        auto identifier = ParseIdentifier();
        identifier.Nest();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForVariableName);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        state.AddChild(std::move(identifier));

        // Now if there is an equal sign, we have an initializer
        if (!m_LexerState.Consume(TokenType::Equal))
        {
            // It can also be the case that it is a C++ style declaration, so we try to parse it, if it wasn't auto
            if (m_LexerState.Consume(TokenType::LeftParenthesis))
            {
                PULSARION_ASSERT(!state.Children.empty(), "There should be at least one child, since we have an identifier");
                if (state.Children[0].Type == NodeType::KeywordAuto)
                {
                    // We can return the result, since it is not a valid expression
                    state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedTypeForInitializerListVariableDeclaration);
                    return state.ToErrorResult(m_LexerState.Peek().Location);
                }

                SyntaxNode argumentList(NodeType::ArgumentList, token.Location);
                do {
                    auto expr = ParseExpression();
                    expr.Nest();

                    if (!expr.Success())
                    {
                        state.AddErrors(std::move(expr));
                        expr.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpressionForInitializerListVariableDeclaration);
                        return state.ToErrorResult(m_LexerState.Peek().Location);
                    }
                    state.AddErrors(std::move(expr));

                    argumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                state.AddChild(std::move(argumentList));

                if (!m_LexerState.Consume(TokenType::RightParenthesis))
                {
                    state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedClosingParenthesisForInitializerListVariableDeclaration);
                    return state.ToErrorResult(m_LexerState.Peek().Location);
                }

                backtrackState.KeepChanges();
                return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableInitialization);
            }

            backtrackState.KeepChanges();
            return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableDeclaration);
        }

        // We have an initializer
        auto initializer = ParseExpression();
        initializer.Nest();
        if (!initializer.Success())
        {
            state.AddErrors(std::move(initializer));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Assignment, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpressionForVariableDefinition);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        backtrackState.KeepChanges();
        state.AddChild(std::move(initializer));
        return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableDefinition);
    }

    Parsing::Result Parser::ParseAnnotation()
    {
        auto backtrackState = m_LexerState.Snapshot();
        InternalParseState state(m_LexerState.Peek());

        // We try to consume double brackets
        if (!m_LexerState.Consume(TokenType::DoubleLeftBracket))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Annotation, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedDoubleLeftBracketForAnnotation);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        // We have double brackets, so we can parse the annotation
        auto identifier = ParseIdentifier(false, false);
        identifier.Nest();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Annotation, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedIdentifierForAnnotation);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }
        state.AddChild(std::move(identifier));

        // We have an identifier, so we can parse the arguments
        if (!m_LexerState.Consume(TokenType::DoubleRightBracket))
        {
            state.Errors.emplace_back(m_LexerState.Peek().Location, Parsing::ErrorSource::Annotation, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedDoubleRightBracketForAnnotation);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::Annotation);
    }

    Parsing::ExpressionResult Parser::ParseExpression(std::uint32_t minPrecedence)
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        auto result = ParseUnaryExpression();
        if (result.Type == Parsing::ExpressionResult::PrimType::Failed)
        {
            // We can return the result, since it is not a valid expression
            return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(result.Errors), std::move(result.Warnings));
        }

        // We have a valid LHS for the expression
        PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!");

        auto lhs = result.Root.value();

        while (true)
        {
            token = m_LexerState.Read();
            auto operatorInfo = GetOperatorInfo(token.Type);

            if (operatorInfo.Precedence < minPrecedence)
            {
                // We don't use goto, so we have to repeat the code
                m_LexerState.Backtrack(1);
                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(result.Type, lhs);
            }

            switch (operatorInfo.type)
            {
            case OperatorInfo::Type::Ternary: {
                if (!result.IsBoolean())
                {
                    // We can return the result, since it is not a valid expression
                    result.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::TernaryOperatorRequiresBooleanCondition);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(result.Errors), std::move(result.Warnings));
                }

                auto trueExpression = ParseExpression(0);
                if (trueExpression.Type == Parsing::ExpressionResult::PrimType::Failed)
                {
                    // We can return the result, since it is not a valid expression
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(trueExpression.Errors), std::move(trueExpression.Warnings));
                }

                if (!m_LexerState.Consume(TokenType::Colon))
                {
                    // We can return the result, since it is not a valid expression
                    trueExpression.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedColonForTernaryOperator);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(trueExpression.Errors), std::move(trueExpression.Warnings));
                }

                auto falseExpression = ParseExpression(0);
                if (falseExpression.Type == Parsing::ExpressionResult::PrimType::Failed)
                {
                    // We can return the result, since it is not a valid expression
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(falseExpression.Errors), std::move(falseExpression.Warnings));
                }

                PULSARION_ASSERT(trueExpression.Root.has_value(), "Root node must have a value when there are no errors!");
                PULSARION_ASSERT(falseExpression.Root.has_value(), "Root node must have a value when there are no errors!");

                if (!trueExpression.CanConvert(falseExpression.Type))
                {
                    // We can return the result, since it is not a valid expression
                    trueExpression.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::TernaryOperatorRequiresCompatibleOperands);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(trueExpression.Errors), std::move(trueExpression.Warnings));
                }

                state.AddChild(std::move(lhs));
                state.AddChild(std::move(trueExpression.Root.value()));
                state.AddChild(std::move(falseExpression.Root.value()));
                auto res = state.CreateNode(NodeType::TernaryOperation, token);
                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(trueExpression.Type, res, std::move(result.Errors), std::move(result.Warnings));
            }
            case OperatorInfo::Type::Logical: {
                if (!result.IsBoolean())
                {
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, {
                        Parsing::Error(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionOperatorRequiresBooleanOperands)
                    });
                }

                BacktrackState rhsBacktrackState = m_LexerState.Snapshot();
                auto rhs = ParseExpression(operatorInfo.Precedence);
                if (!rhs.IsBoolean())
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, {
                        Parsing::Error(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionOperatorRequiresBooleanOperands)
                    });
                rhsBacktrackState.KeepChanges();

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");

                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryBooleanOperation, token);
                state.Children.clear();
                result.Type = Parsing::ExpressionResult::PrimType::Boolean;
                break;
            }
            case OperatorInfo::Type::Comparison: {
                auto rhs = ParseExpression(operatorInfo.Precedence);
                if (rhs.Type == Parsing::ExpressionResult::PrimType::Failed || !result.CanConvert(rhs.Type))
                {
                    // We can return the result, since it is not a valid expression
                    auto errors = std::move(rhs.Errors);
                    if (rhs.Type != Parsing::ExpressionResult::PrimType::Failed)
                        errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionComparisonOperatorRequiresCompatibleOperands);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(errors));
                }

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");
                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryComparisonOperation, token);
                result.Type = Parsing::ExpressionResult::PrimType::Comparison;
                state.Children.clear();
                break;
            }
            case OperatorInfo::Type::Numeric: {
                if (operatorInfo.type != OperatorInfo::Type::Numeric)
                {
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, {
                        Parsing::Error(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionOperatorRequiresNumericOperands)
                    });
                }

                auto rhs = ParseExpression(operatorInfo.Precedence);

                if (!rhs.IsNumeric())
                {
                    // We can return the result, since it is not a valid expression
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, {
                        Parsing::Error(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionOperatorRequiresNumericOperands)
                    });
                }

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");
                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryNumericOperation, token);
                result.Type = Parsing::ExpressionResult::PrimType::Numeric;
                state.Children.clear();
                break;
            }
            case OperatorInfo::Type::Invalid:
                m_LexerState.Backtrack(1);
                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(result.Type, lhs);
            }
        }
    }

    Parsing::ExpressionResult Parser::ParseUnaryExpression()
    {
        #define EXPECT_BOOLEAN(var) if (!(var).IsBoolean()) { state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionExpectsBooleanPrimType); break; }
        #define EXPECT_NUMERIC(var) if (!(var).IsNumeric()) { state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionExpectsNumericPrimType); break; }
        #define PARSE_BOOLEAN(type) { \
            auto result = ParseUnaryExpression(); \
            EXPECT_BOOLEAN(result); \
            PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!"); state.AddChild(std::move(result.Root.value())); \
            backtrackState.KeepChanges(); \
            return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Boolean, state.CreateNode(type), std::move(state.Errors), std::move(state.Warnings)); \
        }
        #define PARSE_NUMERIC(type) { \
            auto result = ParseUnaryExpression(); \
            EXPECT_NUMERIC(result); \
            PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!"); \
            state.AddChild(std::move(result.Root.value())); \
            backtrackState.KeepChanges(); \
            return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Numeric, state.CreateNode(type), std::move(state.Errors), std::move(state.Warnings)); \
        }

        auto backtrackState = m_LexerState.Snapshot();
        // We want to avoid repeating .Consume, so we just backtrack if we fail
        auto token = m_LexerState.Read();
        InternalParseState state(token);

        switch (token.Type)
        {
        case TokenType::Exclamation: PARSE_BOOLEAN(NodeType::BooleanNegation)
        case TokenType::Plus: PARSE_NUMERIC(NodeType::NumericUnaryPlus)
        case TokenType::Minus: PARSE_NUMERIC(NodeType::NumericNegation)
        case TokenType::Increment: PARSE_NUMERIC(NodeType::NumericPreIncrement)
        case TokenType::Decrement: PARSE_NUMERIC(NodeType::NumericPreDecrement)
        case TokenType::Tilde: PARSE_NUMERIC(NodeType::NumericBitwiseNot)
        case TokenType::LeftParenthesis: {
            // Its already consumed, so we don't need to consume it again
            auto result = ParseExpression();
            if (!result.Root.has_value())
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(result.Errors), std::move(result.Warnings));

            if (!m_LexerState.Consume(TokenType::RightParenthesis))
            {
                state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpressionExpectsClosingParenthesis);
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
            }

            state.AddChild(std::move(result.Root.value()));
            backtrackState.KeepChanges();
            return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Undetermined, state.CreateNode(NodeType::ParenthesizedExpression));
        }
        default: {
            // It is probably a primary expression
            backtrackState.UpdateImmediately();

            auto result = ParsePrimaryExpression();
            if (result.Type == Parsing::ExpressionResult::PrimType::Failed)
                return result;

            // Parse postfix operators, like [], ++ and --
            // This is also where we check for function calls
            switch (m_LexerState.Peek().Type)
            {
            case TokenType::LessThan: {
                // TODO: If this fails at any point we just treat it as a normal unary, and not a template
                backtrackState.UpdateBacktrackPoint();

                // Template arguments
                m_LexerState.Consume();
                SyntaxNode templateArgumentList(NodeType::TemplateArgumentList, token.Location);
                do {
                    auto expr = ParsePrimaryExpression(); // Template arguments can only be primary expressions
                    if (!expr.Root.has_value())
                        return result; // This will automatically backtrack

                    state.Errors.splice(state.Errors.end(), std::move(expr.Errors));
                    state.Warnings.splice(state.Warnings.end(), std::move(expr.Warnings));

                    templateArgumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                if (!m_LexerState.Consume(TokenType::GreaterThan))
                    return result; // We just treat it as a normal unary, and it will automatically backtrack

                // Since we fallthrough we need to check the Left Parenthesis
                if (m_LexerState.Peek().Type != TokenType::LeftParenthesis)
                {
                    state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedOpeningParenthesisForFunctionCall);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
                }

                state.AddChild(std::move(templateArgumentList));
             } // Fallthrough
             [[fallthrough]];
            case TokenType::LeftParenthesis: {
                // We consume the left parenthesis
                m_LexerState.Consume();
                state.AddChild(std::move(result.Root.value()));
                // Function name is the first child

                // Should be comma separated expressions or nothing
                if (m_LexerState.Consume(TokenType::RightParenthesis))
                {
                    // We have an empty parameter list
                    backtrackState.KeepChanges();
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Undetermined, state.CreateNode(NodeType::FunctionCall));
                }

                SyntaxNode argumentList(NodeType::ArgumentList, token.Location);
                do {
                    auto expr = ParseExpression();
                    state.Errors.splice(state.Errors.end(), std::move(expr.Errors));
                    state.Warnings.splice(state.Warnings.end(), std::move(expr.Warnings));
                    if (!expr.Root.has_value())
                    {
                        expr.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedExpressionInFunctionCallArgumentList);
                        // We can return the result, since it is not a valid expression
                        return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
                    }

                    argumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                state.AddChild(std::move(argumentList));

                if (!m_LexerState.Consume(TokenType::RightParenthesis))
                {
                    state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedClosingParenthesisForFunctionCallArgumentList);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
                }

                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Undetermined, state.CreateNode(NodeType::FunctionCall));
            }
            case TokenType::LeftBracket: {
                // This is an array index access
                // We consume the left bracket
                m_LexerState.Consume();

                // Array name is the first child
                state.AddChild(std::move(result.Root.value()));

                // Should be a numerical expression
                auto expr = ParseExpression(0);
                state.Errors.splice(state.Errors.end(), std::move(expr.Errors));
                state.Warnings.splice(state.Warnings.end(), std::move(expr.Warnings));
                if (!expr.IsNumeric())
                {
                    expr.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedNumericExpressionForArrayAccess);
                    // We can return the result, since it is not a valid expression
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
                }

                state.AddChild(std::move(expr.Root.value()));

                if (!m_LexerState.Consume(TokenType::RightBracket))
                {
                    state.Errors.emplace_back(state.ToLocation(token.Location), Parsing::ErrorSource::Expression, ErrorSeverity::Fatal, Parsing::ErrorType::ExpectedClosingBracketForArrayAccess);
                    return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));
                }

                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Undetermined, state.CreateNode(NodeType::ArrayIndex));
            }
            case TokenType::Increment: {
                // We consume the increment
                m_LexerState.Consume();
                state.AddChild(std::move(result.Root.value()));
                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Numeric, state.CreateNode(NodeType::NumericPostIncrement));
            }
            case TokenType::Decrement: {
                // We consume the decrement
                m_LexerState.Consume();
                state.AddChild(std::move(result.Root.value()));
                backtrackState.KeepChanges();
                return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Numeric, state.CreateNode(NodeType::NumericPostDecrement));
            }
            default:
                break;
            }

            backtrackState.KeepChanges();
            return result;
        }
        }
        // We leverage the power of the C++ destructor to make sure that we backtrack if we fail
        return Parsing::ExpressionResult(Parsing::ExpressionResult::PrimType::Failed, std::nullopt, std::move(state.Errors), std::move(state.Warnings));

        #undef EXPECT_BOOLEAN
        #undef EXPECT_NUMERIC
        #undef PARSE_BOOLEAN
        #undef PARSE_NUMERIC
    }

    Parsing::ExpressionResult Parser::ParsePrimaryExpression()
    {
        using PrimType = Parsing::ExpressionResult::PrimType;
        auto token = m_LexerState.Peek();
        switch (token.Type)
        {
        case TokenType::NumberInt:
        case TokenType::NumberFloat:
        case TokenType::NumberDouble: // I don't think we will ever support long double
        case TokenType::NumberLong:
        case TokenType::NumberLongLong:
        case TokenType::NumberUnsigned:
        case TokenType::NumberUnsignedLong:
        case TokenType::NumberUnsignedLongLong:
        case TokenType::HexNumber:
        case TokenType::BinaryNumber:
        case TokenType::OctalNumber:
            return Parsing::ExpressionResult(PrimType::Numeric, SyntaxNode(NodeType::NumericLiteral, token.Location, m_LexerState.Read()));
        case TokenType::True:
        case TokenType::False:
            return Parsing::ExpressionResult(PrimType::Boolean, SyntaxNode(NodeType::BooleanLiteral, token.Location, m_LexerState.Read()));
        case TokenType::ColonColon:
        case TokenType::Identifier: {
            auto result = ParseIdentifier();
            if (!result.Root.has_value())
                return Parsing::ExpressionResult(PrimType::Failed);
            return Parsing::ExpressionResult(PrimType::Undetermined, result.Root);
        }
        default:
            return Parsing::ExpressionResult(PrimType::Failed);
        }
    }

    static std::vector<std::string> GenerateScopeForIdentifier(std::vector<std::string>& scope, SyntaxNode& identifier)
    {
        if (identifier.Type != NodeType::Identifier)
            return {};

        // We find a namespace typed child, otherwise the namespace is just the current scope
        std::vector<std::string> result;

        // We clone the scope, since we don't want to modify the original
        result = scope;

        auto it = std::find_if(identifier.Children.begin(), identifier.Children.end(), [](const SyntaxNode& node) { return node.Type == NodeType::Namespace; });
        if (it == identifier.Children.end())
            return result;

        auto& scopeNode = *it;
        // If the first node is a token, then it is the global scope
        if (scopeNode.Children[0].Type == NodeType::Token)
        {
            result.clear();
            scopeNode.Children.erase(it);
        }

        for (auto& node : scopeNode.Children)
        {
            if (node.Type != NodeType::Identifier || !node.Content.has_value())
                continue; // TODO: Probably should add an error here

            result.push_back(node.Content->Value);
        }

        return result;
    }

    // =====================================================================================================================

    // =====================================================================================================================
    // LexerState Functions
    // =====================================================================================================================


    Token Parser::LexerState::Peek(std::size_t offset)
    {
        if (CurrentTokenIndex + offset >= EndOfStreamIndex)
            return Token(TokenType::EndOfFile);

        // For the first token 0 + 0 = 0
        if (CurrentTokenIndex + offset >= TokensRead.size())
            ReadTokens(CurrentTokenIndex + offset - TokensRead.size() + 1);

        return TokensRead[CurrentTokenIndex + offset];
    }

    Token Parser::LexerState::Read()
    {
        if (CurrentTokenIndex >= EndOfStreamIndex)
            return Token(TokenType::EndOfFile);

        Token token = Peek();
        CurrentTokenIndex++;
        return token;
    }

    void Parser::LexerState::Consume(std::size_t count)
    {
        if (CurrentTokenIndex + count >= EndOfStreamIndex)
            return;

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

    bool Parser::LexerState::Consume(TokenType type, Token& token)
    {
        if (Peek().Type == type)
        {
            token = Read();
            return true;
        }

        return false;
    }

    void Parser::LexerState::Backtrack(std::size_t count)
    {
        if (CurrentTokenIndex < count)
        {
            // We try to be tolerant, so we just backtrack to the start
            CurrentTokenIndex = 0;
            return;
        }

        CurrentTokenIndex -= count;
    }

    void Parser::LexerState::BacktrackTo(std::size_t index)
    {
        // We try to be tolerant, so we just return without erroring
        if (index >= EndOfStreamIndex)
            return;

        CurrentTokenIndex = index;
    }

    // Just semantically different from BacktrackTo
    void Parser::LexerState::GoTo(std::size_t index)
    {
        // We try to be tolerant, so we just return without erroring
        if (index >= EndOfStreamIndex)
            return;

        CurrentTokenIndex = index;
    }

    Parser::BacktrackState Parser::LexerState::Snapshot()
    {
        return BacktrackState(*this, CurrentTokenIndex);
    }

    void Parser::LexerState::ReadTokens(std::size_t count)
    {
        for (std::size_t i = 0; i < count;)
        {
            Token token = Lexer.NextToken();
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
