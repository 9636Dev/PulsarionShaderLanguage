#pragma once

#include "Core.hpp"

#include <string>

namespace Pulsarion::Shader
{
    class PULSARION_SHADER_LANGUAGE_API StrUtil
    {
    public:
        StrUtil() = delete; // Only static methods

        /*
         * @brief Escapes the string by replacing special characters with their escape sequences
         * @param str The string to escape
         * @return The escaped string
         */
        static std::string Escape(const std::string& str);

        /*
         * @brief Converts a string into a byte vector
         * @param str The string to convert
         * @return The bytes of the string
         */
        static std::vector<std::uint8_t> ToBytes(const std::string& str);


        /*
         * @brief Converts a byte vector into a string of the bytes' numerical values
         * @param bytes The bytes to convert
         * @return The string of the bytes' numerical values
         */
        static std::string ByteStr(const std::vector<std::uint8_t>& bytes);

        /*
         *  @brief Checks if a string is empty, which allows for newline, tab, space, etc.
         *  @param str The string to check
         *  @return True if the string is empty, false otherwise
         */
        static bool Empty(const std::string& str); // Contains newline, tab, space, etc.
    };
}
