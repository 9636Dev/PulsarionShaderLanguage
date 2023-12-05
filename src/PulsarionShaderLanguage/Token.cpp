#include "Token.hpp"

namespace Pulsarion::Shader
{
    std::string TokenToString(TokenType type)
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
}
