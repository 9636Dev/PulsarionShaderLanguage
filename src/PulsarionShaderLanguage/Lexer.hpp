#include <PulsarionCore/Core.hpp>
#include <PulsarionCore/Log.hpp>
#include <PulsarionCore/File.hpp>

namespace Pulsarion::Shader
{
    enum class TokenType
    {
        EndOfFile,
        Unknown,
        Identifier,
        Number,
        BinaryNumber,
        HexNumber,
        OctalNumber,
        InvalidNumber, // 1.2.3
        Char,
        InvalidChar, // 'a
        LeftParenthesis,
        RightParenthesis,
        LeftBrace,
        RightBrace,
        LeftBracket,
        RightBracket,
        Semicolon,
        Colon,
        ColonColon, // ::
        Comma,
        Dot,
        Plus,
        Minus,
        Asterisk,
        Slash,
        Percent,
        Ampersand,
        Pipe,
        Caret,
        Tilde,
        TildeEqual,
        Exclamation,
        Question,
        LessThan,
        GreaterThan,
        LessThanEqual,
        GreaterThanEqual,
        Equal,
        EqualEqual,
        NotEqual,
        LeftShift,
        RightShift,
        PlusEqual,
        MinusEqual,
        AsteriskEqual,
        SlashEqual,
        PercentEqual,
        AmpersandEqual,
        PipeEqual,
        CaretEqual,
        LeftShiftEqual,
        RightShiftEqual,
        LogicalAnd,
        LogicalOr,
        Increment,
        Decrement,
        Hash,
        Comment,
        InlineComment,

        // Keywords
        If,
        Else,
        For,
        While,
        Do,
        Switch,
        Case,
        Default,
        Break,
        Continue,
        Return,
        True,
        False,
    };

    inline std::string TokenToString(TokenType type)
    {
        switch (type)
        {
        case TokenType::EndOfFile: return "EndOfFile";
        case TokenType::Unknown: return "Unknown";
        case TokenType::Identifier: return "Identifier";
        case TokenType::Number: return "Number";
        case TokenType::BinaryNumber: return "BinaryNumber";
        case TokenType::HexNumber: return "HexNumber";
        case TokenType::OctalNumber: return "OctalNumber";
        case TokenType::InvalidNumber: return "InvalidNumber";
        case TokenType::Char: return "Char";
        case TokenType::InvalidChar: return "InvalidChar";
        case TokenType::LeftParenthesis: return "LeftParenthesis";
        case TokenType::RightParenthesis: return "RightParenthesis";
        case TokenType::LeftBrace: return "LeftBrace";
        case TokenType::RightBrace: return "RightBrace";
        case TokenType::LeftBracket: return "LeftBracket";
        case TokenType::RightBracket: return "RightBracket";
        case TokenType::Semicolon: return "Semicolon";
        case TokenType::Colon: return "Colon";
        case TokenType::ColonColon: return "ColonColon";
        case TokenType::Comma: return "Comma";
        case TokenType::Dot: return "Dot";
        case TokenType::Plus: return "Plus";
        case TokenType::Minus: return "Minus";
        case TokenType::Asterisk: return "Asterisk";
        case TokenType::Slash: return "Slash";
        case TokenType::Percent: return "Percent";
        case TokenType::Ampersand: return "Ampersand";
        case TokenType::Pipe: return "Pipe";
        case TokenType::Caret: return "Caret";
        case TokenType::Tilde: return "Tilde";
        case TokenType::Exclamation: return "Exclamation";
        case TokenType::Question: return "Question";
        case TokenType::LessThan: return "LessThan";
        case TokenType::GreaterThan: return "GreaterThan";
        case TokenType::LessThanEqual: return "LessThanEqual";
        case TokenType::GreaterThanEqual: return "GreaterThanEqual";
        case TokenType::Equal: return "Equal";
        case TokenType::EqualEqual: return "EqualEqual";
        case TokenType::NotEqual: return "NotEqual";
        case TokenType::LeftShift: return "LeftShift";
        case TokenType::RightShift: return "RightShift";
        case TokenType::PlusEqual: return "PlusEqual";
        case TokenType::MinusEqual: return "MinusEqual";
        case TokenType::AsteriskEqual: return "AsteriskEqual";
        case TokenType::SlashEqual: return "SlashEqual";
        case TokenType::PercentEqual: return "PercentEqual";
        case TokenType::AmpersandEqual: return "AmpersandEqual";
        case TokenType::PipeEqual: return "PipeEqual";
        case TokenType::CaretEqual: return "CaretEqual";
        case TokenType::LeftShiftEqual: return "LeftShiftEqual";
        case TokenType::RightShiftEqual: return "RightShiftEqual";
        case TokenType::LogicalAnd: return "LogicalAnd";
        case TokenType::LogicalOr: return "LogicalOr";
        case TokenType::Increment: return "Increment";
        case TokenType::Decrement: return "Decrement";
        case TokenType::Hash: return "Hash";
        case TokenType::Comment: return "Comment";
        case TokenType::InlineComment: return "InlineComment";
        case TokenType::If: return "If";
        case TokenType::Else: return "Else";
        case TokenType::For: return "For";
        case TokenType::While: return "While";
        case TokenType::Do: return "Do";
        case TokenType::Switch: return "Switch";
        case TokenType::Case: return "Case";
        case TokenType::Default: return "Default";
        case TokenType::Break: return "Break";
        case TokenType::Continue: return "Continue";
        case TokenType::Return: return "Return";
        case TokenType::True: return "True";
        case TokenType::False: return "False";
        default: return "UnhandledTokenType";
        }

    }

    struct Token
    {
        TokenType Type;
        std::string Value;
        std::size_t Line;
        std::size_t Column;

        Token(TokenType type, std::string value, std::size_t line, std::size_t column)
            : Type(type), Value(value), Line(line), Column(column)
        {
        }
    };

    class Lexer
    {
    public:
        Lexer(const std::string& source);
        ~Lexer();

        Token NextToken();

    private:
        void SkipWhitespace();
        /// <summary>
        /// Advances the index and increments the line and resets the column.
        /// </summary>
        inline void NewLine();
        /// <summary>
        /// Returns the next character and advances the index. This is implemeneted as source[index+]
        /// </summary>
        inline char NextChar();
        /// <summary>
        /// Returns the current character. This is implemeneted as source[index]
        /// </summary>
        inline char CurrentChar();
        /// <summary>
        /// Returns the next character without advancing the index. This is implemeneted as source[index + 1]
        /// </summary>
        inline char PeekChar();

        Token ReadNumber();
        Token ReadChar();
        Token ReadComment();
        Token ReadIdentifier();

        bool IsDigit(char c);
        bool IsIdentifierStart(char c);

        std::string m_Source;
        std::size_t m_Index = 0;
        std::size_t m_Line = 1;
        std::size_t m_Column = 1;
    };
}
