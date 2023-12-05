#include "Lexer.hpp"

#include <PulsarionCore/Log.hpp>
#include <PulsarionCore/Assert.hpp>

#include <unordered_map>

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
        // Just basic tokens, add more later
    };

    Lexer::Lexer(const std::string& source)
        : m_Source(source)
    {
    }

    Lexer::~Lexer()
    {
    }

    Token Lexer::NextToken()
    {
        SkipWhitespace();

        if (m_Index + 1 >= m_Source.size())
            return Token(TokenType::EndOfFile, "", m_Line, m_Column); // We are at the end of the file, + 1 because we count the character we advance.
    
        const char c = NextChar();
        switch (c)
        {
        case '(':
            return Token(TokenType::LeftParenthesis, "(", m_Line, m_Column);
        case ')':
            return Token(TokenType::RightParenthesis, ")", m_Line, m_Column);
        case '[':
            return Token(TokenType::LeftBracket, "[", m_Line, m_Column);
        case ']':
            return Token(TokenType::RightBracket, "]", m_Line, m_Column);
        case '{':
            return Token(TokenType::LeftBrace, "{", m_Line, m_Column);
        case '}':
            return Token(TokenType::RightBrace, "}", m_Line, m_Column);
        case ';': 
            return Token(TokenType::Semicolon, ";", m_Line, m_Column);
        case ':':
            if (CurrentChar() == ':')
            {
                (void)NextChar();
                return Token(TokenType::ColonColon, "::", m_Line, m_Column);
            }
            return Token(TokenType::Colon, ":", m_Line, m_Column);
        case ',':
            return Token(TokenType::Comma, ",", m_Line, m_Column);
        case '.':
            return Token(TokenType::Dot, ".", m_Line, m_Column);
        case '+':
            if (CurrentChar() == '+')
            {
                (void)NextChar();
                return Token(TokenType::Increment, "++", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::PlusEqual, "+=", m_Line, m_Column);
            }
            return Token(TokenType::Plus, "+", m_Line, m_Column);
        case '-':
            if (CurrentChar() == '-')
            {
                (void)NextChar();
                return Token(TokenType::Decrement, "--", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::MinusEqual, "-=", m_Line, m_Column);
            }
            return Token(TokenType::Minus, "-", m_Line, m_Column);
        case '*':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::AsteriskEqual, "*=", m_Line, m_Column);
            }
            return Token(TokenType::Asterisk, "*", m_Line, m_Column);
        case '/':
            if (CurrentChar() == '/')
            {
                (void)NextChar();
                return ReadComment();
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::SlashEqual, "/=", m_Line, m_Column);
            }
            return Token(TokenType::Slash, "/", m_Line, m_Column);
        case '%':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::PercentEqual, "%=", m_Line, m_Column);
            }
            return Token(TokenType::Percent, "%", m_Line, m_Column);
        case '=':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::EqualEqual, "==", m_Line, m_Column);
            }
            return Token(TokenType::Equal, "=", m_Line, m_Column);
        case '!':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::NotEqual, "!=", m_Line, m_Column);
            }
            return Token(TokenType::Exclamation, "!", m_Line, m_Column);
        case '>':
            if (CurrentChar() == '>') // Shift right
            {
                (void)NextChar();
                if (CurrentChar() == '=')
                {
                    (void)NextChar();
                    return Token(TokenType::RightShiftEqual, ">>=", m_Line, m_Column);
                }
                return Token(TokenType::RightShift, ">>", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::GreaterThanEqual, ">=", m_Line, m_Column);
            }
            return Token(TokenType::GreaterThan, ">", m_Line, m_Column);
        case '<':
            if (CurrentChar() == '<') // Shift left
            {
                (void)NextChar();
                if (CurrentChar() == '=')
                {
                    (void)NextChar();
                    return Token(TokenType::LeftShiftEqual, "<<=", m_Line, m_Column);
                }
                return Token(TokenType::LeftShift, "<<", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::LessThanEqual, "<=", m_Line, m_Column);
            }
            return Token(TokenType::LessThan, "<", m_Line, m_Column);
        case '&':
            if (CurrentChar() == '&')
            {
                (void)NextChar();
                return Token(TokenType::LogicalAnd, "&&", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::AmpersandEqual, "&=", m_Line, m_Column);
            }
            return Token(TokenType::Ampersand, "&", m_Line, m_Column);
        case '|':
            if (CurrentChar() == '|')
            {
                (void)NextChar();
                return Token(TokenType::LogicalOr, "||", m_Line, m_Column);
            }
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::PipeEqual, "|=", m_Line, m_Column);
            }
            return Token(TokenType::Pipe, "|", m_Line, m_Column);
        case '^':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::CaretEqual, "^=", m_Line, m_Column);
            }
            return Token(TokenType::Caret, "^", m_Line, m_Column);
        case '~':
            if (CurrentChar() == '=')
            {
                (void)NextChar();
                return Token(TokenType::TildeEqual, "~=", m_Line, m_Column);
            }
            return Token(TokenType::Tilde, "~", m_Line, m_Column);
        case '?':
            return Token(TokenType::Question, "?", m_Line, m_Column);
        case '#':
            return Token(TokenType::Hash, "#", m_Line, m_Column);
        case '\'':
            return ReadChar();
        }

        if (IsDigit(c))
            return ReadNumber();

        if (IsIdentifierStart(c))
            return ReadIdentifier();

        // Implement later
        return Token(TokenType::Unknown, "", m_Line, m_Column);
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

    char Lexer::PeekChar()
    {
        if (m_Index + 1 >= m_Source.size())
            return '\0'; // It is fine to peek past the end of the source, just return null.

        return m_Source[m_Index + 1];
    }

    char Lexer::CurrentChar()
    {
        PULSARION_ASSERT(m_Index < m_Source.size(), "Lexer reached end of source but continued to read!");
        return m_Source[m_Index];
    }

    char Lexer::NextChar()
    {
        PULSARION_ASSERT(m_Index < m_Source.size(), "Lexer reached end of source but continued to read!");
        char c = m_Source[m_Index++];
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
        std::size_t start = m_Index; 
        while (m_Index < m_Source.size() && m_Source[m_Index] != '\n')
        {
            (void)NextChar();
        }
        if (m_Index >= m_Source.size())
        {
            return Token(TokenType::Comment, m_Source.substr(start, m_Index - start), m_Line, m_Column);
        }
        // Otherwise call NewLine() and return the comment.
        size_t end = m_Index; // So we don't include the newline in the comment.
        NewLine();
        return Token(TokenType::Comment, m_Source.substr(start, end - start), m_Line, m_Column);
    }

    Token Lexer::ReadChar()
    {
        // Should be called after the ' character has been read.
        std::size_t start = m_Index;
        std::size_t column = m_Column;
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
                return Token(TokenType::InvalidChar, m_Source.substr(start, m_Index - start),m_Line, column);
            }
        }

        // Read the closing ' character.
        if (NextChar() != '\'')
        {
            return Token(TokenType::InvalidChar, m_Source.substr(start, m_Index - start), m_Line, column);
        }

        // Convert the character to a string and return it.
        std::string str = "";
        str += c;
        return Token(TokenType::Char, str, m_Line, column);
    }

    Token Lexer::ReadNumber()
    {
        // The starting character is index - 1 because we already read the first character.
        std::size_t start = m_Index - 1;
        std::size_t column = m_Column - 1;
        if (m_Source[start] == '0')
        {
            char c = CurrentChar();
            switch (c)
            {
            case 'x':
            case 'X':
                // Hexadecimal number
                (void)NextChar();
                while (IsDigit(CurrentChar()) || (CurrentChar() >= 'a' && CurrentChar() <= 'f') || (CurrentChar() >= 'A' && CurrentChar() <= 'F'))
                {
                    (void)NextChar();
                }
                return Token(TokenType::HexNumber, m_Source.substr(start, m_Index - start), m_Line, column);
            case 'b':
            case 'B':
                // Binary number
                (void)NextChar();
                while (CurrentChar() == '0' || CurrentChar() == '1')
                {
                    (void)NextChar();
                }
                return Token(TokenType::BinaryNumber, m_Source.substr(start, m_Index - start), m_Line, column);
            case 'o':
            case 'O':
                // Octal number
                (void)NextChar();
                while (CurrentChar() >= '0' && CurrentChar() <= '7')
                {
                    (void)NextChar();
                }
                return Token(TokenType::OctalNumber, m_Source.substr(start, m_Index - start), m_Line, column);
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
                {
                    return Token(TokenType::InvalidNumber, m_Source.substr(start, m_Index - start), m_Line, column); 
                }
                hasDecimal = true;
            }
            if (CurrentChar() == 'e' || CurrentChar() == 'E')
            {
                if (hasExponent)
                {
                    return Token(TokenType::InvalidNumber, m_Source.substr(start, m_Index - start), m_Line, column); 
                }
                hasExponent = true;
                // The next character can be '+' or '-'.
                (void)NextChar();
                if (CurrentChar() == '+' || CurrentChar() == '-')
                {
                    (void)NextChar();
                }
            }
            (void)NextChar();
        }
        // If ended with 'e' or 'E' then it is invalid.
        if (m_Source[m_Index - 1] == 'e' || m_Source[m_Index - 1] == 'E')
        {
            return Token(TokenType::InvalidNumber, m_Source.substr(start, m_Index - start), m_Line, column); 
        }
        
        if (m_Index >= m_Source.size())
        {
            // Broken because EOF
            return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
        }

        // It could end with f, F, d, D, l, L, u, U, ul, UL, lu, LU, ll, LL, or ull, ULL.
        
        if (CurrentChar() == 'f' || CurrentChar() == 'F')
        {
            (void)NextChar();
            return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
        }

        if (CurrentChar() == 'd' || CurrentChar() == 'D')
        {
            (void)NextChar();
            return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
        }

        if (CurrentChar() == 'l' || CurrentChar() == 'L')
        {
            NextChar();
            return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
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
                    return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
                }
                return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
            }
            return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
        }

        // Otherwise it is a valid number, unless it is followed by alpha character.
        if (IsIdentifierStart(CurrentChar()))
        {
            return Token(TokenType::InvalidNumber, m_Source.substr(start, m_Index - start), m_Line, column); 
        }
        return Token(TokenType::Number, m_Source.substr(start, m_Index - start), m_Line, column);
    }

    Token Lexer::ReadIdentifier()
    {
        std::size_t start = m_Index - 1;
        std::size_t column = m_Column - 1;
        while (m_Index < m_Source.size() && (IsIdentifierStart(CurrentChar()) || IsDigit(CurrentChar())))
        {
            (void)NextChar();
        }

        std::string identifier = m_Source.substr(start, m_Index - start);
        
        // Check if it is a keyword.
        auto it = LiteralTokens.find(identifier);
        if (it != LiteralTokens.end())
        {
            return Token(it->second, identifier, m_Line, column);
        }

        return Token(TokenType::Identifier, identifier, m_Line, column);
    };
}
