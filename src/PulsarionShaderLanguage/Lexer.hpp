#pragma once
#include <PulsarionCore/Core.hpp>
#include <PulsarionCore/Log.hpp>
#include <PulsarionCore/File.hpp>

#include "Core.hpp"
#include "Token.hpp"

namespace Pulsarion::Shader
{

    class PULSARION_SHADER_LANGUAGE_API Lexer
    {
    public:
        Lexer(std::string source);
        ~Lexer();

        Token NextToken();

        std::size_t GetPosition() const { return m_Index; }
        bool IsEnd() const { return m_Index >= m_Source.size(); }

        static bool IsDigit(char c);
        static bool IsIdentifierStart(char c);
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
        inline char CurrentChar() const;
        /// <summary>
        /// Returns the next character without advancing the index. This is implemeneted as source[index + 1]
        /// </summary>
        inline char PeekChar() const;

        Token ReadNumber();
        Token ReadChar();
        Token ReadComment();
        Token ReadIdentifier();

        std::string m_Source;
        std::size_t m_Index = 0;
        std::size_t m_Line = 1;
        std::size_t m_Column = 1;
    };
}
