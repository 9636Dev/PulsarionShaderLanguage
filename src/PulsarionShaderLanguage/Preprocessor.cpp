#include "Preprocessor.hpp"
#include "PulsarionCore/File.hpp"
#include <unordered_map>
#include <filesystem>
#include <string>
#include <set>

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

    static std::optional<std::filesystem::path> ParsePath(const std::string& str)
    {
        if (str.size() < 2 || str.front() != '"' && str.front() != '<')
            return std::nullopt;

        char closing = str.front() == '"' ? '"' : '<';
        std::size_t end = str.find(closing, 1);
        if (end == std::string::npos || end != str.size() - 1) // We didn't find the closing or there's something after it
            return std::nullopt;
        return std::filesystem::path(str.substr(1, end - 1));
    };

    struct FileInfo
    {
        std::filesystem::path Path;
        std::string Source;
        std::size_t Index; // Store the state of the file so we don't use recursion
        std::size_t Line;

        FileInfo(std::filesystem::path path, std::string source, std::size_t index, std::size_t line = 1)
            : Path(std::move(path)), Source(std::move(source)), Index(index), Line(line)
        {
        }
    };

    std::string Preprocessor::ErrorToString(Error error)
    {
        switch (error)
        {
        case Error::None:
            return "No error";
        case Error::ReadFileFailed:
            return "Failed to read file";
        case Error::FileNotFound:
            return "File not found";
        case Error::InvalidDirective:
            return "Invalid directive";
        case Error::InvalidCondition:
            return "Invalid condition";
        case Error::InvalidPragma:
            return "Invalid pragma";
        case Error::CharsAfterDirective:
            return "Unexpected characters after the directive";
        }
        return "Unknown error";
    }

    Preprocessor::Result Preprocessor::Process() const
    {
        std::set<std::filesystem::path> includedFiles;
        std::stack<FileInfo> fileStack;
        std::stack<bool> conditionStack;
        fileStack.emplace(m_Path, m_Source, 0, 1);
        std::string result;

        std::size_t reserved = m_Source.size();
        result.reserve(reserved); // At least the same size

        while (!fileStack.empty())
        {
            FileInfo& fileInfo = fileStack.top();
            std::string& source = fileInfo.Source;
            std::size_t& index = fileInfo.Index;
            std::size_t& line = fileInfo.Line;

            while (index < source.size() && fileStack.top().Path == fileInfo.Path) // If the stack changed we should stop
            {
                SkipWhitespace(source, result, index);
                if (index >= source.size())
                    break;

                // Should be the first character of a line
                if (source[index] == '#')
                {
                    auto directive = ReadWord(source, ++index);
                    if (directive == "include")
                    {
                        SkipWhitespace(source, index);
                        auto path = ParsePath(ReadWord(source, index));
                        if (!path.has_value())
                            return Result(result, Error::InvalidDirective, 0, fileInfo.Path);
                        File file(fileInfo.Path.parent_path() / *path); // Current file's directory
                        if (!file.Exists())
                            return Result(result, Error::FileNotFound, 0, file.GetPath());
                        std::string includeSource;
                        if (!file.Read(includeSource))
                            return Result(result, Error::ReadFileFailed, 0, file.GetPath());
                        reserved += includeSource.size();
                        result.reserve(reserved);
                        fileStack.emplace(file.GetPath(), std::move(includeSource), 0);
                    }
                    else if (directive == "pragma")
                    {
                        SkipWhitespace(source, index);
                        auto pragma = ReadWord(source, index);
                        if (pragma == "once")
                        {
                            if (includedFiles.contains(fileInfo.Path))
                            {
                                fileStack.pop();
                                continue;
                            }
                            else
                                includedFiles.insert(fileInfo.Path);
                        }
                        else
                            return Result(result, Error::InvalidPragma, line, fileInfo.Path);
                    }
                    else if (directive == "ifdef")
                    {
                        SkipWhitespace(source, index);
                        auto macro = ReadWord(source, index);
                        conditionStack.push(m_Defines.contains(macro));
                    }
                    else if (directive == "ifndef")
                    {
                        SkipWhitespace(source, index);
                        auto macro = ReadWord(source, index);
                        conditionStack.push(!m_Defines.contains(macro));
                    }
                    else if (directive == "else")
                    {
                        if (conditionStack.empty())
                            return Result(result, Error::InvalidCondition, line, fileInfo.Path);
                        conditionStack.top() = !conditionStack.top();
                    }
                    else if (directive == "endif")
                        conditionStack.pop();
                    else if (directive == "define")
                    {
                        SkipWhitespace(source, index);
                        auto macro = ReadWord(source, index);
                        SkipWhitespace(source, index);
                        std::size_t definitionStart = index;
                        while (index < source.size() && (source[index] != '\n' && source[index] != '\r'))
                            ++index;
                        std::size_t definitionEnd = index;
                        if (definitionEnd - definitionStart > 0)
                            m_Defines[macro] = source.substr(definitionStart, definitionEnd - definitionStart);
                        else
                            m_Defines[macro] = "1";
                    }
                    else if (directive == "undef")
                    {
                        SkipWhitespace(source, index);
                        auto macro = ReadWord(source, index);
                        m_Defines.erase(macro);
                    }
                    else if (directive == "debug")
                    {
                        // This directive can be used to display current states when preprocessing
                        SkipWhitespace(source, index);
                        auto word = ReadWord(source, index);
                        if (word == "defines")
                        {
                            PULSARION_LOG_INFO("[{0}:{1}] Defines:", fileInfo.Path.string(), line);
                            for (const auto& [key, value] : m_Defines)
                                PULSARION_LOG_INFO("  {} = {}", key, value);
                        }
                        else
                            return Result(result, Error::InvalidDirective, line, fileInfo.Path);
                    }
                    else
                        return Result(result, Error::InvalidDirective, line, fileInfo.Path); // TODO: include SourceLocation

                    if (index >= source.size())
                        break;
                    SkipWhitespace(source, index);
                    if (source[index] != '\n' && source[index] != '\r')
                        return Result(result, Error::CharsAfterDirective, line, fileInfo.Path); // Unexpected characters after the directive

                    if (source[index++] == '\r')
                    {
                        if (index < source.size() && source[index] == '\n')
                            ++index;
                    }

                    line++;
                    if (conditionStack.empty() || conditionStack.top())
                        result += '\n';

                    continue; // The line should be only a directive
                }

                while (index < source.size() && (source[index] != '\n' && source[index] != '\r'))
                {
                    if (conditionStack.empty() || conditionStack.top())
                        result += source[index];
                    ++index;
                }
                if (index >= source.size())
                    break;

                PULSARION_ASSERT(source[index] == '\n' || source[index] == '\r', "We should be at the end of the line");
                if (source[index++] == '\r')
                {
                    if (index < source.size() && source[index] == '\n')
                        ++index;
                }

                if (conditionStack.empty() || conditionStack.top())
                    result += '\n';
                line++;

                // Now we can start a new line
            }

            if (fileStack.top().Path == fileInfo.Path) // If the stack changed we shouldn't pop
                fileStack.pop();
        }

        // TODO: LineOffsets
        return Result(result, Error::None, 0, m_Path);
    }
}
