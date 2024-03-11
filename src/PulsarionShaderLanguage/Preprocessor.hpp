#pragma once

#include "Core.hpp"
#include <string>
#include <filesystem>
#include <unordered_map>
#include <utility>
#include <vector>

namespace Pulsarion::Shader
{
    class PULSARION_SHADER_LANGUAGE_API Preprocessor
    {
    public:
        enum Error
        {
            None,
            ReadFileFailed,
            FileNotFound,
            InvalidDirective,
            InvalidCondition,
            InvalidPragma,
            CharsAfterDirective
        };

        struct Result
        {
            std::string Source;
            Error Err;
            std::size_t Line; // Line of the error or the last line
            std::filesystem::path Path; // Also src for the error

            [[nodiscard]] bool HasError() const { return Err != Error::None; }

            Result(std::string source, Error error, std::size_t line, std::filesystem::path path) : Source(std::move(source)), Err(error), Line(line), Path(std::move(path))  {}
        };
        explicit Preprocessor(std::string source, const std::filesystem::path& path) : m_Source(std::move(source)), m_Path(std::filesystem::absolute(path)) {}

        [[nodiscard]] static std::string ErrorToString(Error error);
        [[nodiscard]] Result Process() const;

    private:
        std::string m_Source;
        std::filesystem::path m_Path;
        mutable std::unordered_map<std::string, std::string> m_Defines;
    };
}
