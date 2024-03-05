#include "Preprocessor.hpp"
#include "PulsarionCore/File.hpp"
#include "PulsarionCore/Log.hpp"
#include <unordered_map>
#include <filesystem>
#include <string>

namespace Pulsarion::Shader
{
    static void SkipWhitespace(const std::string& source, std::size_t& index)
    {
        while (index < source.size() && (source[index] == ' ' || source[index] == '\t'))
        {
            ++index;
        }
    }

    static void SkipWhitespace(const std::string& source, std::string& result, std::size_t& sourceIndex)
    {
        while (sourceIndex < source.size() && (source[sourceIndex] == ' ' || source[sourceIndex] == '\t'))
        {
            result += source[sourceIndex];
            ++sourceIndex;
        }
    }

    static std::string ReadWord(const std::string& source, std::size_t& index)
    {
        std::size_t start = index;
        while (index < source.size() && source[index] != ' ' && source[index] != '\t' && source[index] != '\n' && source[index] != '\r')
        {
            ++index;
        }

        return source.substr(start, index - start);
    }

    static bool ProcessCondition(const std::string& condition, std::unordered_map<std::string, std::string>& defines)
    {
        return defines.find(condition) != defines.end();
    }

    static std::string ProcessFile(const std::string& source, const std::filesystem::path& path, std::unordered_map<std::string, std::string>& defines)
    {
        std::string result;
        result.reserve(source.size());

        std::size_t curIndex = 0;
        bool skipLine = false;

        while (curIndex < source.size())
        {
            SkipWhitespace(source, result, curIndex);

            if (source[curIndex] == '#')
            {
                auto directive = ReadWord(source, ++curIndex);
                if (directive == "pragma")
                {
                    SkipWhitespace(source, curIndex);
                    auto pragma = ReadWord(source, curIndex);
                    if (pragma == "once")
                    {
                        std::string define = "__INTERNAL__PRAGMAONCE_FILE_" + path.string() + "__";
                        if (defines.find(define) != defines.end())
                            return ""; // Skip the entire file
                        defines[define] = "1";
                    }
                }
                else if (directive == "define")
                {
                    SkipWhitespace(source, curIndex);
                    auto define = ReadWord(source, curIndex);
                    SkipWhitespace(source, curIndex);
                    auto value = ReadWord(source, curIndex);
                    defines[define] = value; // Overwrite or set new define
                }
                else if (directive == "include")
                {
                    SkipWhitespace(source, curIndex);
                    auto includePath = ReadWord(source, curIndex);
                    // Make sure it starta and ends with either " or <
                    if (includePath.front() != '"' && includePath.front() != '<')
                    {
                        PULSARION_LOG_ERROR("Invalid include path: {}", includePath);
                        return "";
                    }

                    if (includePath.front() != includePath.back())
                    {
                        PULSARION_LOG_ERROR("Invalid include path: {}", includePath);
                        return "";
                    }
                    File file((path.parent_path() / std::filesystem::path(includePath.substr(1, includePath.size() - 2))).string());
                    if (!file.Exists())
                    {
                        PULSARION_LOG_ERROR("Failed to include file: {}", includePath);
                        return "";
                    }

                    std::string includeSource;
                    if (!file.Read(includeSource))
                    {
                        PULSARION_LOG_ERROR("Failed to read included file: {}", includePath);
                        return "";
                    }

                    std::string processedIncludeSource = ProcessFile(includeSource, file.GetPath(), defines);

                    // Append the processed content to the result instead of returning it
                    result.append(processedIncludeSource);
                }
                else if (directive == "ifdef") // TODO: Does't work at all
                {
                    SkipWhitespace(source, curIndex);
                    auto condition = ReadWord(source, curIndex);
                    skipLine = !ProcessCondition(condition, defines);
                }
                else if (directive == "ifndef")
                {
                    SkipWhitespace(source, curIndex);
                    auto condition = ReadWord(source, curIndex);
                    skipLine = ProcessCondition(condition, defines);
                }
                else if (directive == "endif")
                {
                    skipLine = false; // Nested won't work, we need to fix that later
                    // TODO: Fix nested ifdefs
                }
                else if (directive == "else")
                {
                    skipLine = !skipLine;
                }
            }
            else if (!skipLine)
            {
                while (curIndex < source.size() && source[curIndex] != '\n')
                {
                    result += source[curIndex++];
                }
            }

            if (source[curIndex] == '\n')
            {
                if (!skipLine) result += source[curIndex];
                ++curIndex; // Move past the newline character
                skipLine = false; // Reset skipping logic for new lines
            }
        }

        return result;
    }

    [[nodiscard]] std::string Preprocessor::Process() const
    {
        return ProcessFile(m_Source, m_Path, m_Defines);
    }
}
