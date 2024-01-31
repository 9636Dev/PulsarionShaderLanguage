#include "Lexer.hpp"

#include <PulsarionCore/Log.hpp>
#include <PulsarionCore/Assert.hpp>

#include <unordered_map>
#include <utility>

namespace Pulsarion::Shader
{
    static std::unordered_map<std::string, TokenType> LiteralTokens = {
        { "true", TokenType::True },
        { "false", TokenType::False },
        { "if", TokenType::If },
        { "else", TokenType::Else },
        { "for", TokenType::For },
        { "while", TokenType::While },
        { "do", TokenType::Do },
        { "break", TokenType::Break },
        { "return", TokenType::Return },
        { "continue", TokenType::Continue },
        { "switch", TokenType::Switch },
        { "case", TokenType::Case },
        { "default", TokenType::Default },
        { "struct", TokenType::Struct },
        { "using", TokenType::Using },
        { "auto", TokenType::Auto },
        { "namespace", TokenType::Namespace },
        // Just basic tokens, add more later
    };

    Lexer::Lexer(std::string source)
        : m_Source(std::move(source))
    {
    }

    Lexer::~Lexer() = default;

    Token Lexer::NextToken()
    {
        SkipWhitespace();

        if (m_Index + 1 >= m_Source.size())
            return {TokenType::EndOfFile, "", m_Line, m_Column, m_Index}; // We are at the end of the file, + 1 because we count the character we advance.

        const char c = NextChar();
        std::size_t column = m_Column - 1;
        std::size_t index = m_Index - 1;
        switch (c)
        {
        case '(':
            return {TokenType::LeftParenthesis, "(", m_Line, column, index};
        case ')':
            return {TokenType::RightParenthesis, ")", m_Line, column, index};
        case '[':
            if (CurrentChar() == '[')
            {
                (void)NextChar();
                return {TokenType::DoubleLeftBracket, "[[", m_Line, column, index};
            }
            return {TokenType::LeftBracket, "[", m_Line, column, index};
        case ']':
            if (CurrentChar() == ']')
            {
                (void)NextChar();
                return {TokenType::DoubleRightBracket, "]]", m_Line, column, index};
            }
            return {TokenType::RightBracket, "]", m_Line, column, index};
        case '{':
            return {TokenType::LeftBrace, "{", m_Line, column, index};
        case '}':
            return {TokenType::RightBrace, "}", m_Line, column, index};
        case ';':
            return {TokenType::Semicolon, ";", m_Line, column, index};
        case ':':
            if (CurrentChar() == ':')
            {
                (void)NextChar();
                return {TokenType::ColonColon, "::", m_Line, column, index};
            }
            return {TokenType::Colon, ":", m_Line, column, index};
        case ',':
            return {TokenType::Comma, ",", m_Line, column, index};
        case '.':
            return {TokenType::Dot, ".", m_Line, column, index};
        case '+':
            if (CurrentChar() == '+')
            {
                (void)NextChar();
                return {TokenType::Increment, "++", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::PlusEqual, "+=", m_Line, column, index};
            }
            return {TokenType::Plus, "+", m_Line, column, index};
        case '-':
            if (CurrentChar() == '-')
            {
                (void)NextChar();
                return {TokenType::Decrement, "--", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::MinusEqual, "-=", m_Line, column, index};
            }
            return {TokenType::Minus, "-", m_Line, column, index};
        case '*':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::AsteriskEqual, "*=", m_Line, column, index};
            }
            return {TokenType::Asterisk, "*", m_Line, column, index};
        case '/':
            if (CurrentChar() == '/')
            {
                (void)NextChar();
                return ReadComment();
            }
            if (CurrentChar() == '*')
            {
                (void)NextChar();
                return ReadInlineComment();
            }

            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::SlashEqual, "/=", m_Line, column, index};
            }
            return {TokenType::Slash, "/", m_Line, column, index};
        case '%':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::PercentEqual, "%=", m_Line, column, index};
            }
            return {TokenType::Percent, "%", m_Line, column, index};
        case '=':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::EqualEqual, "==", m_Line, column, index};
            }
            return {TokenType::Equal, "=", m_Line, column, index};
        case '!':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::NotEqual, "!=", m_Line, column, index};
            }
            return {TokenType::Exclamation, "!", m_Line, column, index};
        case '>':
            if (CurrentChar() == '>') // Shift right
            {
                (void)NextChar();
                if (CurrentChar() == '=')
                {
                    (void)NextChar();
                    return {TokenType::RightShiftEqual, ">>=", m_Line, column, index};
                }
                return {TokenType::RightShift, ">>", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::GreaterThanEqual, ">=", m_Line, column, index};
            }
            return {TokenType::GreaterThan, ">", m_Line, column, index};
        case '<':
            if (CurrentChar() == '<') // Shift left
            {
                (void)NextChar();
                if (CurrentChar() == '=')
                {
                    (void)NextChar();
                    return {TokenType::LeftShiftEqual, "<<=", m_Line, column, index};
                }
                return {TokenType::LeftShift, "<<", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::LessThanEqual, "<=", m_Line, column, index};
            }
            return {TokenType::LessThan, "<", m_Line, column, index};
        case '&':
            if (CurrentChar() == '&')
            {
                (void)NextChar();
                return {TokenType::LogicalAnd, "&&", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::AmpersandEqual, "&=", m_Line, column, index};
            }
            return {TokenType::Ampersand, "&", m_Line, column, index};
        case '|':
            if (CurrentChar() == '|')
            {
                (void)NextChar();
                return {TokenType::LogicalOr, "||", m_Line, column, index};
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::PipeEqual, "|=", m_Line, column, index};
            }
            return {TokenType::Pipe, "|", m_Line, column, index};
        case '^':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::CaretEqual, "^=", m_Line, column, index};
            }
            return {TokenType::Caret, "^", m_Line, column, index};
        case '~':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return {TokenType::TildeEqual, "~=", m_Line, column, index};
            }
            return {TokenType::Tilde, "~", m_Line, column, index};
        case '?':
            return {TokenType::Question, "?", m_Line, column, index};
        case '#':
            return {TokenType::Hash, "#", m_Line, column, index};
        case '\'':
            return ReadChar();
        default:
            break;
        }

        if (IsDigit(c))
            return ReadNumber();

        if (IsIdentifierStart(c))
            return ReadIdentifier();

        // Implement later
        return {TokenType::Unknown, "", m_Line, column, index};
    }

    void Lexer::SkipWhitespace()
    {
        while (m_Index < m_Source.size())
        {
            switch (m_Source[m_Index])
            {
            case ' ':
            case '\t':
                (void)NextChar();
                break;
            case '\r':
                // If there is a \n skip that as well and call NewLine. We want to support CR, LF, and CRLF.
                if (PeekChar() == '\n')
                    (void)NextChar();
                NewLine();
                break;
            case '\n':
                NewLine();
                break;
            default:
                return;
            }
        }
    }

    char Lexer::PeekChar() const
    {
        if (m_Index + 1 >= m_Source.size())
            return '\0'; // It is fine to peek past the end of the source, just return null.

        return m_Source[m_Index + 1];
    }

    char Lexer::CurrentChar() const
    {
        PULSARION_ASSERT(m_Index < m_Source.size(), "Lexer reached end of source but continued to read!");
        return m_Source[m_Index];
    }

    char Lexer::NextChar()
    {
        PULSARION_ASSERT(m_Index < m_Source.size(), "Lexer reached end of source but continued to read!");
        const char c = m_Source[m_Index++];
        m_Column++;
        return c;
    }

    void Lexer::NewLine()
    {
        PULSARION_ASSERT(m_Index < m_Source.size(), "Lexer reached end of source but continued to advance lines!");
        m_Line++;
        m_Column = 0;
        m_Index++;
    }

    bool Lexer::IsDigit(char c)
    {
        return c >= '0' && c <= '9';
    }

    bool Lexer::IsIdentifierStart(char c)
    {
        return (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               c == '_';
    }

    Token Lexer::ReadComment()
    {
        const std::size_t startIndex = m_Index;
        const std::size_t startColumn = m_Column;

        while (m_Index < m_Source.size() && (m_Source[m_Index] != '\n' && m_Source[m_Index] != '\r'))
        {
            (void)NextChar();
        }

        if (m_Index >= m_Source.size())
            return {TokenType::Comment, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        // Otherwise call NewLine() and return the comment.
        const size_t end = m_Index; // So we don't include the newline in the comment.
        NewLine();
        return {TokenType::Comment, m_Source.substr(startIndex, end - startIndex), m_Line, startColumn, startIndex};
    }

    Token Lexer::ReadInlineComment()
    {
        const std::size_t startIndex = m_Index;
        const std::size_t startColumn = m_Column;

        // TODO: Doesn't work
        while (m_Index < m_Source.size() && (m_Source[m_Index] != '*'))
        {
            (void)NextChar();
        }

        if (m_Index >= m_Source.size()) // This is an error, we reached the end of the file without finding the end of the comment.
            return {TokenType::InvalidComment, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        const size_t end = m_Index; // So we don't include the newline in the comment.
        (void)NextChar(); // Skip the '*'

        if (m_Index >= m_Source.size())
            return {TokenType::InvalidComment, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        if (PeekChar() != '/')
            return {TokenType::InvalidComment, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        (void)NextChar(); // Skip the '/'
        return {TokenType::InlineComment, m_Source.substr(startIndex, end - startIndex), m_Line, startColumn, startIndex};
    }


    Token Lexer::ReadChar()
    {
        // Should be called after the ' character has been read.
        const std::size_t startIndex = m_Index;
        const std::size_t startColumn = m_Column;
        char c = NextChar();
        if (c == '\\')
        {
            // Escape sequence
            c = NextChar();
            switch (c)
            {
            case 'n':
                c = '\n';
                break;
            case 't':
                c = '\t';
                break;
            case 'r':
                c = '\r';
                break;
            case '\\':
                c = '\\';
                break;
            case '\'':
                c = '\'';
                break;
            case '"':
                c = '"';
                break;
            case '0':
                c = '\0';
                break;
            default:
                return {TokenType::InvalidChar, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
            }
        }

        // Read the closing ' character.
        if (NextChar() != '\'')
        {
            return {TokenType::InvalidChar, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        // Convert the character to a string and return it.
        std::string str;
        str += c;
        return {TokenType::Char, str, m_Line, startColumn, startIndex};
    }

    Token Lexer::ReadNumber()
    {
        // The starting character is index - 1 because we already read the first character.
        const std::size_t startIndex = m_Index - 1;
        const std::size_t startColumn = m_Column - 1;
        if (m_Source[startIndex] == '0')
        {
            switch (CurrentChar())
            {
            case 'x':
            case 'X':
                // Hexadecimal number
                (void)NextChar();
                while (IsDigit(CurrentChar()) || (CurrentChar() >= 'a' && CurrentChar() <= 'f') || (CurrentChar() >= 'A' && CurrentChar() <= 'F'))
                {
                    (void)NextChar();
                }
                return {TokenType::HexNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
            case 'b':
            case 'B':
                // Binary number
                (void)NextChar();
                while (CurrentChar() == '0' || CurrentChar() == '1')
                {
                    (void)NextChar();
                }
                return {TokenType::BinaryNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
            case 'o':
            case 'O':
                // Octal number
                (void)NextChar();
                while (CurrentChar() >= '0' && CurrentChar() <= '7')
                {
                    (void)NextChar();
                }
                return {TokenType::OctalNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
            default:
                break;
            }
        }

        // It is just a normal base 10 number, but maybe it is a float with/without 'e'.
        // Make sure there is only one decimal point.
        bool hasDecimal = false;
        bool hasExponent = false;
        while (m_Index < m_Source.size() && (IsDigit(CurrentChar()) || CurrentChar() == '.' || CurrentChar() == 'e' || CurrentChar() == 'E'))
        {
            if (CurrentChar() == '.')
            {
                if (hasDecimal)
                    return {TokenType::InvalidNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

                hasDecimal = true;
            }
            if (CurrentChar() == 'e' || CurrentChar() == 'E')
            {
                if (hasExponent)
                    return {TokenType::InvalidNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

                hasExponent = true;
                // The next character can be '+' or '-'.
                (void)NextChar();
                if (CurrentChar() == '+' || CurrentChar() == '-')
                    (void)NextChar();
            }
            (void)NextChar();
        }
        // If ended with 'e' or 'E' then it is invalid.
        if (m_Source[m_Index - 1] == 'e' || m_Source[m_Index - 1] == 'E')
        {
            return {TokenType::InvalidNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        if (m_Index >= m_Source.size()) // Broken because EOF
            return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        // It could end with f, F, d, D, l, L, u, U, ul, UL, lu, LU, ll, LL, or ull, ULL.

        if (CurrentChar() == 'f' || CurrentChar() == 'F')
        {
            (void)NextChar();
            return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        if (CurrentChar() == 'd' || CurrentChar() == 'D')
        {
            (void)NextChar();
            return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        if (CurrentChar() == 'l' || CurrentChar() == 'L')
        {
            NextChar();
            return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        if (CurrentChar() == 'u' || CurrentChar() == 'U')
        {
            NextChar();
            if (CurrentChar() == 'l' || CurrentChar() == 'L')
            {
                NextChar();
                if (CurrentChar() == 'l' || CurrentChar() == 'L')
                {
                    NextChar();
                    return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
                }
                return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
            }
            return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
        }

        // Otherwise it is a valid number, unless it is followed by alpha character.
        if (IsIdentifierStart(CurrentChar()))
            return {TokenType::InvalidNumber, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};

        return {TokenType::Number, m_Source.substr(startIndex, m_Index - startIndex), m_Line, startColumn, startIndex};
    }

    Token Lexer::ReadIdentifier()
    {
        std::size_t startIndex = m_Index - 1;
        std::size_t startColumn = m_Column - 1;
        while (m_Index < m_Source.size() && (IsIdentifierStart(CurrentChar()) || IsDigit(CurrentChar())))
        {
            (void)NextChar();
        }

        std::string identifier = m_Source.substr(startIndex, m_Index - startIndex);

        // Check if it is a keyword.
        const auto it = LiteralTokens.find(identifier);
        if (it != LiteralTokens.end())
            return {it->second, identifier, m_Line, startColumn, startIndex};


        return {TokenType::Identifier, identifier, m_Line, startColumn, startIndex};
    };
}
