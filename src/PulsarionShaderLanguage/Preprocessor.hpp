#pragma once

#include <string>
#include <filesystem>
#include <unordered_map>

namespace Pulsarion::Shader
{
    class Preprocessor
    {
    public:
        explicit Preprocessor(std::string source, std::filesystem::path path) : m_Source(std::move(source)), m_Path(std::move(path)) {}

        [[nodiscard]] std::string Process() const;

    private:
        std::string m_Source;
        std::filesystem::path m_Path;
        mutable std::unordered_map<std::string, std::string> m_Defines;
    };
}
