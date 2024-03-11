#include "StrUtil.hpp"

namespace Pulsarion::Shader
{
    std::string StrUtil::Escape(const std::string& str)
    {
        std::string result;
        result.reserve(str.size());
        for (char c : str)
        {
            switch (c)
            {
            case '\n':
                result += "\\n";
                break;
            case '\r':
                result += "\\r";
                break;
            case '\t':
                result += "\\t";
                break;
            case '\v':
                result += "\\v";
                break;
            case '\f':
                result += "\\f";
                break;
            case '\a':
                result += "\\a";
                break;
            case '\b':
                result += "\\b";
                break;
            case '\\':
                result += "\\\\";
                break;
            case '\'':
                result += "\\\'";
                break;
            case '\"':
                result += "\\\"";
                break;
            default:
                result += c;
                break;
            }
        }
        return result;
    };

    std::vector<std::uint8_t> StrUtil::ToBytes(const std::string& str)
    {
        std::vector<std::uint8_t> result;
        result.reserve(str.size());
        for (char c : str)
            result.push_back(static_cast<std::uint8_t>(c));
        return result;
    }

    std::string StrUtil::ByteStr(const std::vector<std::uint8_t>& bytes)
    {
        // We print the bytes as numbers, not as characters
        std::string result;
        result.reserve(bytes.size() * 4);
        for (std::uint8_t byte : bytes)
            result += std::to_string(byte) + ' ';
        return result;
    }

    bool StrUtil::Empty(const std::string& str)
    {
        for (char c : str)
        {
            if (c != ' ' && c != '\t' && c != '\n' && c != '\r' && c != '\v' && c != '\f' && c != '\a' && c != '\b')
                return false;
        }
        return true;
    }
}
