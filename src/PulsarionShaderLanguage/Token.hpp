#pragma once

#include "Core.hpp"

#include <string>
#include <utility>
#include <utility>

namespace Pulsarion::Shader
{
    enum class TokenType
    {
        EndOfFile,
        Unknown,
        Identifier,
        NumberInt,
        NumberFloat,
        NumberDouble,
        NumberLong,
        NumberLongLong,
        NumberUnsigned,
        NumberUnsignedLong,
        NumberUnsignedLongLong,
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
        DoubleLeftBracket, // [[ for annotations
        DoubleRightBracket,
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
        InvalidComment, // When a comment is not closed

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
        Struct,
        True,
        False,
        Using,
        Auto,
        Namespace,
    };

    PULSARION_SHADER_LANGUAGE_API std::string TokenTypeToString(TokenType type);

    struct PULSARION_SHADER_LANGUAGE_API Token
    {
        TokenType Type;
        std::string Value;
        SourceLocation Location;

        Token(TokenType type, std::string value, std::size_t line, std::size_t column, std::size_t index)
            : Type(type), Value(std::move(value)), Location(line, column, index, Value.size())
        {
        }

        Token(TokenType type)
            : Token(type, "", 0, 0, 0)
        {
        }

        Token() : Token(TokenType::Unknown, "", 0, 0, 0)
        {

        }

        std::string ToString() const;

        bool operator==(const Token& other) const
        {
            return Type == other.Type && Value == other.Value;
        }
    };

}
