#include <gtest/gtest.h>
#include <PulsarionShaderLanguage/Lexer.hpp>

using namespace Pulsarion::Shader;

// We make a test fixture to avoid having to create a new Lexer for each test.
class LexerTest : public ::testing::Test
{
protected:
    std::string source;

    Lexer lexer();

    LexerTest()
    {
    }

    void SetUp() override
    {
    }
};

TEST_F(LexerTest, TestCommentParsing)
{
    std::string src =
            "// This is a regular comment\n"
            "/* This is a regular inline comment */\n"
            "// /* This should still be parsed as a regular comment */\n"
            "/* This is a regular comment with a // in it */\n"
            "/* This is an invalid comment that doesnt have the closing";

    Lexer lexer(src);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::Comment);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InlineComment);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::Comment);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InlineComment);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidComment);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::EndOfFile);
}
