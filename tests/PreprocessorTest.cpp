#include <gtest/gtest.h>
#include <filesystem>

#include "PulsarionCore/File.hpp"
#include "PulsarionShaderLanguage/Preprocessor.hpp"
#include "PulsarionShaderLanguage/StrUtil.hpp"

using namespace Pulsarion::Shader;
namespace fs = std::filesystem;

struct PreprocessorTestParam
{
    std::filesystem::path Path;
    Preprocessor::Error Err;
    bool IsEmpty;

    PreprocessorTestParam(std::filesystem::path path, Preprocessor::Error err, bool isEmpty)
        : Path(std::move(path)), Err(err), IsEmpty(isEmpty)
    {
    }
};

class PreprocessorTest : public testing::TestWithParam<PreprocessorTestParam>
{
};

TEST_P(PreprocessorTest, Test)
{
    auto param = GetParam();
    auto file = Pulsarion::File::ReadAllText(param.Path);
    if (file.empty())
        throw std::runtime_error("Failed to read file: " + param.Path.string());

    Preprocessor preprocessor(file, param.Path);
    auto result = preprocessor.Process();
    if (param.Err == Preprocessor::Error::None && result.HasError())
        std::cerr << "Unexpected Error: " << Preprocessor::ErrorToString(result.Err) << "(" << result.Path.string() << ":" << result.Line << ")" << std::endl;
    ASSERT_EQ(param.Err, result.Err);
    bool empty = StrUtil::Empty(result.Source);
    if (param.IsEmpty && !empty)
        std::cerr << "Source Not Empty: " << result.Source << std::endl;
    ASSERT_EQ(param.IsEmpty, empty);
}

INSTANTIATE_TEST_SUITE_P(PreprocessorTest, PreprocessorTest, testing::Values(
    PreprocessorTestParam { fs::path("resources/tests/preprocessor/test_include.pshl"), Preprocessor::Error::None, true },
    PreprocessorTestParam { fs::path("resources/tests/preprocessor/test_define.pshl"), Preprocessor::Error::None, true }
));
