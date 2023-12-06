#pragma once

#include "Core.hpp"

#include <string>

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

    PULSARION_SHADER_LANGUAGE_API std::string TokenToString(TokenType type);

    struct Token
    {
        TokenType Type;
        std::string Value;
        std::size_t Line;
        std::size_t Column;
        std::size_t Index;

        Token(TokenType type, std::string value, std::size_t line, std::size_t column, std::size_t index)
            : Type(type), Value(value), Line(line), Column(column), Index(index)
        {
        }

        Token() : Token(TokenType::Unknown, "", 0, 0, 0)
        {

        }
    };

}
