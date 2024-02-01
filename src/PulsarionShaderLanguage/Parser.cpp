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
            return OperatorInfo(10, OperatorInfo::Type::Numeric);
        case TokenType::Plus:
        case TokenType::Minus:
            return OperatorInfo(9, OperatorInfo::Type::Numeric);
        case TokenType::LeftShift:
        case TokenType::RightShift:
            return OperatorInfo(8, OperatorInfo::Type::Numeric);
        case TokenType::LessThan:
        case TokenType::LessThanEqual:
        case TokenType::GreaterThan:
        case TokenType::GreaterThanEqual:
            return OperatorInfo(7, OperatorInfo::Type::Comparison);
        case TokenType::EqualEqual:
        case TokenType::NotEqual:
            return OperatorInfo(6, OperatorInfo::Type::Comparison);
        case TokenType::Ampersand:
            return OperatorInfo(5, OperatorInfo::Type::Logical);
        case TokenType::Caret:
            return OperatorInfo(4, OperatorInfo::Type::Logical);
        case TokenType::Pipe:
            return OperatorInfo(3, OperatorInfo::Type::Logical);
        case TokenType::LogicalAnd:
            return OperatorInfo(2, OperatorInfo::Type::Logical);
        case TokenType::LogicalOr:
            return OperatorInfo(1, OperatorInfo::Type::Logical);
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
        std::list<ParserError> Errors;
        std::list<ParserError> Warnings;

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

        void AddChild(ParseResult&& result)
        {
            PULSARION_ASSERT(result.Success(), "The result must be successful to add it as a child!")
            AddChild(std::move(result.Root.value()));
            AddErrors(std::move(result));
        }

        void AddErrors(ParseResult&& result)
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

        ParseResult ToResult(const SourceLocation &endLocation, const NodeType nodeType, std::optional<Token>&& nodeContent = std::nullopt)
        {
            Location = ToLocation(endLocation);
            return ParseResult(CreateNode(nodeType, std::move(nodeContent)), Errors, Warnings);
        }

        ParseResult ToErrorResult(const SourceLocation &endLocation)
        {
            Location = ToLocation(endLocation);
            return ParseResult(std::nullopt, Errors, Warnings);
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

        if (result.Errors.empty())
        {
            result.Errors.emplace_back(result.Root->Location, ParserError::ErrorSource::Scope, ErrorSeverity::Fatal, ErrorType::UnexpectedExtraClosingBrace);
            return result;
        }

        auto it = std::find_if(result.Errors.begin(), result.Errors.end(),
            [](const ParserError& error) { return error.Type == ErrorType::UnexpectedExtraClosingBrace && error.NestingLevel == 0; });
        if (it != result.Errors.end())
        {
            // We remove the error, because we don't expect a closing brace
            result.Errors.remove(*it);
        }

        if (result.Errors.empty())
            return result;

        // It has fails since there are other errors, but for now we just return the result
        return result;
    }

    Parser::ParseResult Parser::ParseScope()
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
                state.Errors.push_back(ParserError(state.ToLocation(token.Location), ParserError::ErrorSource::Scope,
                    ErrorSeverity::Fatal, ErrorType::UnexpectedEndOfFileWhenFindingClosingBrace));
                return state.ToResult(token.Location, NodeType::Scope);
            default: {
                    auto result = ParseStatement();
                    result.Nest();

                    if (result.Success())
                    {
                        // We do one more check, so we don't add any blank lines
                        if (result.Root->Children.size() > 0)
                            state.AddChild(std::move(result.Root.value()));
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

    Parser::ParseResult Parser::ParseStatement()
    {
        InternalParseState state(m_LexerState.Peek());

        BacktrackState backtrackState = m_LexerState.Snapshot();
        backtrackState.KeepChanges(); // By default we keep changes
        ParseResult result = ParseAssignment();
        if (ShouldReturnStatement(result, true, state, backtrackState))
            return result;

        backtrackState.UpdateBacktrackPoint();
        result = ParseDeclarations();
        if (ShouldReturnStatement(result, true, state, backtrackState))
            return result;

        backtrackState.UpdateBacktrackPoint();
        result = ParseStruct();
        if (ShouldReturnStatement(result, false, state, backtrackState))
            return result;

        backtrackState.UpdateBacktrackPoint();
        result = ParseFunction(); state.Errors.splice(state.Errors.end(), result.Errors);
        if (ShouldReturnStatement(result, false, state, backtrackState))
            return result;

        result = ParseExpression();
        if (ShouldReturnStatement(result, true, state, backtrackState))
        {
            state.AddChild(std::move(result));
            return state.ToResult(m_LexerState.Peek().Location, NodeType::Statement);
        }

        return state.ToErrorResult(m_LexerState.Peek().Location);
    }

    bool Parser::ShouldReturnStatement(ParseResult &result, bool requireEOS, InternalParseState& state, BacktrackState& backtrackState)
    {
        if (!result.Success())
        {
            result.Nest();
            state.AddErrors(std::move(result));
            return false;
        }

        const auto token = m_LexerState.Peek();
        if (const auto tokenInfo = StatementHelper::GetTokenInfo(token.Type);
            requireEOS && !tokenInfo.IsEndOfStatement)
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Statement,
                ErrorSeverity::Fatal, ErrorType::ExpectedEndOfStatement);

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


    Parser::ParseResult Parser::ParseExpression()
    {
        const auto result = ParseExpression(0);
        return ParseResult(result.Root, result.Errors, result.Warnings);
    }

    Parser::ParseResult Parser::ParseDeclarations()
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
                state.Errors.emplace_back(identifier.Root->Location, ParserError::ErrorSource::Declaration,
                    ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForReturnType);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }

             // The first value of a function declaration is the return type
            state.AddChild(std::move(identifier));
        }

        // Now we parse the name of the function or struct, we don't support templates yet
        auto identifier = ParseIdentifier();
        if (!identifier.Success())
        {
            state.AddErrors(std::move(identifier));
            state.Errors.emplace_back(identifier.Root->Location, ParserError::ErrorSource::Declaration,
                ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForReturnType);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

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
            state.Errors.emplace_back(loc, ParserError::ErrorSource::Declaration, ErrorSeverity::Fatal, ErrorType::ExpectedOpeningParenthesisForFunctionArguments);
            return state.ToErrorResult(loc);
        }

        auto argumentList = SyntaxNode(NodeType::FunctionArgumentList, token.Location);
        do
        {
            auto argumentType = ParseIdentifier(true, false);
            if (!argumentType.Success())
            {
                argumentType.Nest();
                state.AddErrors(std::move(argumentType));
                state.Errors.emplace_back(argumentType.Root->Location, ParserError::ErrorSource::Declaration,
                    ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForArgumentType);
                return state.ToErrorResult(m_LexerState.Peek().Location);
            }

            argumentList.Children.emplace_back(std::move(argumentType.Root.value()));
        } while (m_LexerState.Consume(TokenType::Comma));

        if (!m_LexerState.Consume(TokenType::RightParenthesis))
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Declaration,
                ErrorSeverity::Fatal, ErrorType::ExpectedClosingParenthesisForFunctionArguments);
            return state.ToErrorResult(m_LexerState.Peek().Location);
        }

        state.AddChild(std::move(argumentList));

        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::FunctionDeclaration);
    }

    Parser::ParseResult Parser::ParseIdentifier(bool allowNamespace, bool allowTrailing)
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
                state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Identifier,
                    ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForIdentifier);
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
                    state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Identifier,
                        ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForIdentifier);
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
                state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Identifier,
                    ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForIdentifier);
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
                    state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Identifier,
                        ErrorSeverity::Fatal, ErrorType::ExpectedIdentifierForIdentifier);
                    return state.ToErrorResult(token.Location);
                }
                trailingNode.Children.emplace_back(NodeType::Token, token.Location, token);
            }
        }
        else if (m_LexerState.Consume(TokenType::Dot, token))
        {
            state.Errors.emplace_back(state.ToLocation(token.Location), ParserError::ErrorSource::Identifier,
                ErrorSeverity::Fatal, ErrorType::UnexpectedDotInNonTrailingIdentifier);
            return state.ToErrorResult(token.Location);
        }

        if (namespaceNode.Children.size() > 0)
            state.AddChild(std::move(namespaceNode));
        if (trailingNode.Children.size() > 0)
            state.AddChild(std::move(trailingNode));

        backtrackState.KeepChanges();
        return state.ToResult(token.Location, NodeType::Identifier, nodeRoot);
    }

    Parser::ParseResult Parser::ParseStruct()
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        // We have a struct keyword
        if (!m_LexerState.Consume(TokenType::Struct))
        {
            state.Errors.emplace_back(token.Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, ErrorType::ExpectedStructKeyword);
            return state.ToErrorResult(token.Location);
        }

        // We have a struct keyword, so we can parse the name
        auto identifier = ParseIdentifier(true, false); // We allow namespaces, but not trailing
        if (!identifier.Success())
        {
            identifier.Nest();
            state.Errors.emplace_back()
        }

        if (!m_LexerState.Consume(TokenType::LeftBrace))
        {
            // We can return the result, since it is not a valid expression
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Expected opening brace") }, 0);
        }

        auto scope = ParseScope();
        if (!scope.Root.has_value() || scope.ErrorFlags != 0) // TODO: Maybe implement better system
            return scope;

        for (auto& child : scope.Root.value().Children)
        {
            switch (child.Type)
            {
            case NodeType::VariableDeclaration:
                // We don't support definitions yet
                child.Type = NodeType::StructMemberVariable;
                break;
            case NodeType::VariableDefinition:
                return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Variable definitions are not allowed in structs yet (Not implemented)") }, 0);
            case NodeType::FunctionDefinition:
                child.Type = NodeType::StructMemberFunction;
                break;
            case NodeType::FunctionDeclaration:
                return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Function declarations are not allowed in structs yet (Headers are not implemented)") }, 0);
            case NodeType::StructDeclaration:
                return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Struct declarations are not allowed in structs yet") }, 0);
            case NodeType::StructDefinition:
                return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Struct definitions are not allowed in structs yet") }, 0);
            default:
                return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Struct, ErrorSeverity::Fatal, "Unexpected node type in struct") }, 0);
            }
        }

        // We have a valid struct, so we can return the result
        state.AddChild(std::move(identifier.Root.value()));
        state.AddChild(std::move(scope.Root.value()));
        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::StructDefinition);
    }

    Parser::ParseResult Parser::ParseFunction()
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
        if (!returnType.Root.has_value())
            return returnType;

        // We have an identifier which is the function name
        auto functionName = ParseIdentifier(true, false);
        if (!functionName.Root.has_value())
            return functionName;

        if (!m_LexerState.Consume(TokenType::LeftParenthesis))
        {
            // We can return the result, since it is not a valid expression
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Function, ErrorSeverity::Fatal, "Expected opening parenthesis") }, 0);
        }

        // We have a valid function declaration, we have a list of 2 identifiers separated by commas
        SyntaxNode argumentList(NodeType::FunctionArgumentList, token.Location);
        do {
            auto argumentType = ParseIdentifier(true, false);
            if (!argumentType.Root.has_value())
                return argumentType;
            // The name can only be one identifier token
            auto argumentName = ParseIdentifier(false, false);
            if (!argumentName.Root.has_value())
                return argumentName;

            SyntaxNode argument(NodeType::FunctionArgument, token.Location);
            argument.Children.emplace_back(std::move(argumentType.Root.value()));
            argument.Children.emplace_back(std::move(argumentName.Root.value()));

            argumentList.Children.emplace_back(std::move(argument));
        } while (m_LexerState.Consume(TokenType::Comma));

        if (!m_LexerState.Consume(TokenType::RightParenthesis))
        {
            // We can return the result, since it is not a valid expression
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Function, ErrorSeverity::Fatal, "Expected closing parenthesis") }, 0);
        }

        // We don't support inline functions, so we expect a scope
        if (!m_LexerState.Consume(TokenType::LeftBrace))
        {
            // We can return the result, since it is not a valid expression
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Function, ErrorSeverity::Fatal, "Expected opening brace") }, 0);

        }

        auto scope = ParseScope();
        if (!scope.Root.has_value())
            return scope;

        state.AddChild(std::move(returnType.Root.value()));
        state.AddChild(std::move(functionName.Root.value()));
        state.AddChild(std::move(argumentList));
        state.AddChild(std::move(scope.Root.value()));

        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::FunctionDefinition);
    }

    // TODO: In the future come up with a way to determine relevance of errors
    Parser::ParseResult Parser::ParseAssignment()
    {
        InternalParseState state(m_LexerState.Peek());
        auto backtrackState = m_LexerState.Snapshot();

        // First we parse declaration, since it is a subset of assignment
        auto result = ParseDeclaration();
        if (result.Root.has_value())
        {
            backtrackState.KeepChanges();
            return result;
        }

        // We don't have a declaration, so we try to parse an assignment
        auto identifier = ParseIdentifier();
        if (!result.Root.has_value())
            return result;

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
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Assignment, ErrorSeverity::Fatal, "Expected valid assignment operator") }, 0);
        }

        auto expression = ParseExpression();
        if (!expression.Root.has_value())
            return expression;
        state.AddChild(std::move(identifier.Root.value()));
        state.AddChild(std::move(expression.Root.value()));
        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, type, token);
    }

    Parser::ParseResult Parser::ParseDeclaration()
    {
        auto token = m_LexerState.Peek();
        InternalParseState state(token);
        auto backtrackState = m_LexerState.Snapshot();

        // There could be annotations, so we try to parse them first
        auto annotation = ParseAnnotation();
        if (annotation.Root.has_value())
            state.AddChild(std::move(annotation.Root.value()));

        // Auto will require type deduction
        if (!m_LexerState.Consume(TokenType::Auto))
        {
            auto result = ParseIdentifier();
            if (!result.Root.has_value())
                return result;
            state.AddChild(std::move(result.Root.value()));
        }
        else
            state.AddChild(SyntaxNode(NodeType::KeywordAuto, token.Location));

        // We have a valid type for the assignment.

        // There should be another identifier, we current don't support unpackings auto [a, b] = ...
        auto identifier = ParseIdentifier();
        if (!identifier.Root.has_value())
            return identifier;
        state.AddChild(std::move(identifier.Root.value()));

        // Now if there is an equal sign, we have an initializer
        if (!m_LexerState.Consume(TokenType::Equal))
        {
            // It can also be the case that it is a C++ style declaration, so we try to parse it, if it wasn't auto
            if (m_LexerState.Consume(TokenType::LeftParenthesis))
{
                PULSARION_ASSERT(state.Children.size() > 0, "There should be at least one child, since we have an identifier");
                if (state.Children[0].Type == NodeType::KeywordAuto)
                {
                    // We can return the result, since it is not a valid expression
                    return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Assignment, ErrorSeverity::Fatal, "Expected type for variable declaration using initializer list") }, 0);
                }

                SyntaxNode argumentList(NodeType::ArgumentList, token.Location);
                do {
                    auto expr = ParseExpression();
                    if (!expr.Root.has_value())
                    {
                        expr.Errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected expression in initialization list");
                        // We can return the result, since it is not a valid expression
                        state.Errors.splice(state.Errors.end(), expr.Errors);
                        return state.ToErrorResult(m_LexerState.Peek().Location);
                    }

                    argumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                state.AddChild(std::move(argumentList));

                if (!m_LexerState.Consume(TokenType::RightParenthesis))
                {
                    // We can return the result, since it is not a valid expression
                    return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Assignment, ErrorSeverity::Fatal, "Expected closing parenthesis") }, 0);
                }

                backtrackState.KeepChanges();
                return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableInitialization);
            }

            backtrackState.KeepChanges();
            return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableDeclaration);
        }

        // We have an initializer
        auto initializer = ParseExpression();
        if (!initializer.Root.has_value())
            return initializer;

        backtrackState.KeepChanges();
        state.AddChild(std::move(initializer.Root.value()));
        return state.ToResult(m_LexerState.Peek().Location, NodeType::VariableDefinition);
    }

    Parser::ParseResult Parser::ParseAnnotation()
    {
        auto backtrackState = m_LexerState.Snapshot();
        InternalParseState state(m_LexerState.Peek());

        // We try to consume double brackets
        if (!m_LexerState.Consume(TokenType::DoubleLeftBracket))
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Annotation, ErrorSeverity::Fatal, "Expected double left bracket") }, 0);

        // We have double brackets, so we can parse the annotation
        Token token = m_LexerState.Read();
        if (token.Type != TokenType::Identifier)
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Annotation, ErrorSeverity::Fatal, "Expected identifier") }, 0);

        // We have an identifier, so we can parse the arguments
        if (!m_LexerState.Consume(TokenType::DoubleRightBracket))
            return ParseResult(std::nullopt, { ParserError(m_LexerState.Peek().Location, ParserError::ErrorSource::Annotation, ErrorSeverity::Fatal, "Expected double right bracket") }, 0);


        state.AddChild(SyntaxNode(token));
        backtrackState.KeepChanges();
        return state.ToResult(m_LexerState.Peek().Location, NodeType::Annotation);
    }

    Parser::ExpressionParseResult Parser::ParseExpression(std::uint32_t minPrecedence)
    {
        auto backtrackState = m_LexerState.Snapshot();
        auto token = m_LexerState.Peek();
        InternalParseState state(token);

        auto result = ParseUnaryExpression();
        if (result.Type == ExpressionParseResult::PrimType::Failed)
        {
            // We can return the result, since it is not a valid expression
            return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, result.Errors);
        }

        // We have a valid LHS for the expression
        PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!");

        auto lhs = result.Root.value();

        while (true)
        {
            token = m_LexerState.Read();
            auto operatorInfo = GetOperatorInfo(token.Type);
            switch (operatorInfo.type)
            {
            case OperatorInfo::Type::Logical: {
                if (!result.IsBoolean() || operatorInfo.Precedence < minPrecedence)
                {
                    // We don't use goto, so we have to repeat the code
                    m_LexerState.Backtrack(1);
                    backtrackState.KeepChanges();
                    return ExpressionParseResult(result.Type, lhs);
                }

                BacktrackState rhsBacktrackState = m_LexerState.Snapshot();
                auto rhs = ParseExpression(operatorInfo.Precedence);
                if (!rhs.IsBoolean())
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, {
                        ParserError(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected boolean expression", 0)
                    });
                rhsBacktrackState.KeepChanges();

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");

                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryBooleanOperation, token);
                state.Children.clear();
                result.Type = ExpressionParseResult::PrimType::Boolean;
                break;
            }
            case OperatorInfo::Type::Comparison: {
                // They can bool bool or numberic numberic comparisons
                if (operatorInfo.Precedence < minPrecedence)
                {
                    m_LexerState.Backtrack(1);
                    backtrackState.KeepChanges();
                    return ExpressionParseResult(result.Type, lhs);
                }

                auto rhs = ParseExpression(operatorInfo.Precedence);
                if (rhs.Type == ExpressionParseResult::PrimType::Failed || !result.CanConvert(rhs.Type))
                {
                    // We can return the result, since it is not a valid expression
                    auto errors = std::move(rhs.Errors);
                    if (rhs.Type != ExpressionParseResult::PrimType::Failed)
                        errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Cannot convert LHS and RHS of operator", 0);
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);
                }

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");
                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryComparisonOperation, token);
                result.Type = ExpressionParseResult::PrimType::Comparison;
                state.Children.clear();
                break;
            }
            case OperatorInfo::Type::Numeric: {
                if (operatorInfo.type != OperatorInfo::Type::Numeric || operatorInfo.Precedence < minPrecedence)
                {
                    m_LexerState.Backtrack(1);
                    backtrackState.KeepChanges();
                    return ExpressionParseResult(result.Type, lhs);
                }

                auto rhs = ParseExpression(operatorInfo.Precedence);

                if (!rhs.IsNumeric())
                {
                    // We can return the result, since it is not a valid expression
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, { ParserError(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected numeric expression", 0) });
                }

                PULSARION_ASSERT(rhs.Root.has_value(), "Root node must have a value when there are no errors!");
                state.AddChild(std::move(lhs));
                state.AddChild(std::move(rhs.Root.value()));
                lhs = state.CreateNode(NodeType::BinaryNumericOperation, token);
                result.Type = ExpressionParseResult::PrimType::Numeric;
                state.Children.clear();
                break;
            }
            case OperatorInfo::Type::Invalid:
                m_LexerState.Backtrack(1);
                backtrackState.KeepChanges();
                return ExpressionParseResult(result.Type, lhs);
            }
        }
    }

    Parser::ExpressionParseResult Parser::ParseUnaryExpression()
    {
        #define EXPECT_BOOLEAN(var) if (!var.IsBoolean()) { errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected boolean expression"); break; }
        #define EXPECT_NUMERIC(var) if (!var.IsNumeric()) { errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected numeric expression"); break; }
        #define PARSE_BOOLEAN(type) { \
            auto result = ParseUnaryExpression(); \
            EXPECT_BOOLEAN(result); \
            PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!"); state.AddChild(std::move(result.Root.value())); \
            backtrackState.KeepChanges(); \
            return ExpressionParseResult(ExpressionParseResult::PrimType::Boolean, state.CreateNode(type)); \
        }
        #define PARSE_NUMERIC(type) { \
            auto result = ParseUnaryExpression(); \
            EXPECT_NUMERIC(result); \
            PULSARION_ASSERT(result.Root.has_value(), "Root node must have a value when there are no errors!"); \
            state.AddChild(std::move(result.Root.value())); \
            backtrackState.KeepChanges(); \
            return ExpressionParseResult(ExpressionParseResult::PrimType::Numeric, state.CreateNode(type)); \
        }

        auto backtrackState = m_LexerState.Snapshot();
        // We want to avoid repeating .Consume, so we just backtrack if we fail
        auto token = m_LexerState.Read();
        InternalParseState state(token);
        std::list<ParserError> errors;

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
                return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, result.Errors);

            if (!m_LexerState.Consume(TokenType::RightParenthesis))
            {
                errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected closing parenthesis");
                return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);
            }

            state.AddChild(std::move(result.Root.value()));
            backtrackState.KeepChanges();
            return ExpressionParseResult(ExpressionParseResult::PrimType::Undetermined, state.CreateNode(NodeType::ParenthesizedExpression));
        }
        default: {
            // It is probably a primary expression
            backtrackState.UpdateImmediately();

            auto result = ParsePrimaryExpression();
            if (result.Type == ExpressionParseResult::PrimType::Failed)
                return result;

            // Parse postfix operators, like [], ++ and --
            // This is also where we check for function calls
            switch (m_LexerState.Peek().Type)
            {
            case TokenType::LessThan: {
                // Template arguments
                m_LexerState.Consume();
                SyntaxNode templateArgumentList(NodeType::TemplateArgumentList, token.Location);
                do {
                    auto expr = ParsePrimaryExpression(); // Template arguments can only be primary expressions
                    if (!expr.Root.has_value())
                    {
                        expr.Errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected expression in template argument list");
                        // We can return the result, since it is not a valid expression
                        return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, expr.Errors);
                    }

                    templateArgumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                if (!m_LexerState.Consume(TokenType::GreaterThan))
                {
                    errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected closing angle bracket");
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);
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
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Undetermined, state.CreateNode(NodeType::FunctionCall));
                }

                SyntaxNode argumentList(NodeType::ArgumentList, token.Location);
                do {
                    auto expr = ParseExpression();
                    if (!expr.Root.has_value())
                    {
                        expr.Errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected expression in function call");
                        // We can return the result, since it is not a valid expression
                        return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, expr.Errors);
                    }

                    argumentList.Children.push_back(std::move(expr.Root.value()));
                } while (m_LexerState.Consume(TokenType::Comma));

                state.AddChild(std::move(argumentList));

                if (!m_LexerState.Consume(TokenType::RightParenthesis))
                {
                    errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected closing parenthesis");
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);
                }

                backtrackState.KeepChanges();
                return ExpressionParseResult(ExpressionParseResult::PrimType::Undetermined, state.CreateNode(NodeType::FunctionCall));
            }
            case TokenType::LeftBracket: {
                // This is an array index access
                // We consume the left bracket
                m_LexerState.Consume();

                // Array name is the first child
                state.AddChild(std::move(result.Root.value()));

                // Should be a numerical expression
                auto expr = ParseExpression(0);
                if (!expr.IsNumeric())
                {
                    expr.Errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected numeric expression in array index access");
                    // We can return the result, since it is not a valid expression
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, expr.Errors);
                }

                state.AddChild(std::move(expr.Root.value()));

                if (!m_LexerState.Consume(TokenType::RightBracket))
                {
                    errors.emplace_back(state.CreateLocation(token.Location), ParserError::ErrorSource::Expression, ErrorSeverity::Fatal, "Expected closing bracket");
                    return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);
                }

                backtrackState.KeepChanges();
                return ExpressionParseResult(ExpressionParseResult::PrimType::Undetermined, state.CreateNode(NodeType::ArrayIndex));
            }
            case TokenType::Increment: {
                // We consume the increment
                m_LexerState.Consume();
                state.AddChild(std::move(result.Root.value()));
                backtrackState.KeepChanges();
                return ExpressionParseResult(ExpressionParseResult::PrimType::Numeric, state.CreateNode(NodeType::NumericPostIncrement));
            }
            case TokenType::Decrement: {
                // We consume the decrement
                m_LexerState.Consume();
                state.AddChild(std::move(result.Root.value()));
                backtrackState.KeepChanges();
                return ExpressionParseResult(ExpressionParseResult::PrimType::Numeric, state.CreateNode(NodeType::NumericPostDecrement));
            }
            default:
                break;
            }

            backtrackState.KeepChanges();
            return result;
        }
        }
        // We leverage the power of the C++ destructor to make sure that we backtrack if we fail
        return ExpressionParseResult(ExpressionParseResult::PrimType::Failed, std::nullopt, errors);

        #undef EXPECT_BOOLEAN
        #undef EXPECT_NUMERIC
        #undef PARSE_BOOLEAN
        #undef PARSE_NUMERIC
    }

    Parser::ExpressionParseResult Parser::ParsePrimaryExpression()
    {
        using PrimType = ExpressionParseResult::PrimType;
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
            return ExpressionParseResult(PrimType::Numeric, SyntaxNode(NodeType::NumericLiteral, token.Location, m_LexerState.Read()));
        case TokenType::True:
        case TokenType::False:
            return ExpressionParseResult(PrimType::Boolean, SyntaxNode(NodeType::BooleanLiteral, token.Location, m_LexerState.Read()));
        case TokenType::ColonColon:
        case TokenType::Identifier: {
            auto result = ParseIdentifier();
            if (!result.Root.has_value())
                return ExpressionParseResult(PrimType::Failed);
            return ExpressionParseResult(PrimType::Undetermined, std::move(result.Root));
        }
        default:
            return ExpressionParseResult(PrimType::Failed);
        }
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
        if (index >= EndOfStreamIndex)
        {
            // We try to be tolerant, so we just return without erroring
            return;
        }

        CurrentTokenIndex = index;
    }

    // Just semantically different from BacktrackTo
    void Parser::LexerState::GoTo(std::size_t index)
    {
        if (index >= EndOfStreamIndex)
        {
            // We try to be tolerant, so we just return without erroring
            return;
        }

        CurrentTokenIndex = index;
    }

    Parser::BacktrackState Parser::LexerState::Snapshot()
    {
        return BacktrackState(*this, CurrentTokenIndex);
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
