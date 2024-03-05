#include <gtest/gtest.h>
#include <PulsarionShaderLanguage/Lexer.hpp>

using namespace Pulsarion::Shader;

// We make a test fixture to avoid having to create a new Lexer for each test.
class LexerTest : public ::testing::Test
{
protected:
    LexerTest() = default;
};

TEST_F(LexerTest, TestInit)
{
    const std::string emptyStr;
    Lexer lexer(emptyStr);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::EndOfFile);

    const std::string stringWithUnicode = "\u00A9";
    Lexer lexer2(stringWithUnicode);
    EXPECT_EQ(lexer2.NextToken().Type, TokenType::Unknown);
    EXPECT_EQ(lexer2.NextToken().Type, TokenType::EndOfFile);

    // We might exceed the stack size with this one.
    const std::string veryLongString(1000000, 'a');
    Lexer lexer3(veryLongString);
    EXPECT_EQ(lexer3.NextToken().Type, TokenType::Identifier); // a * 1000000

    const std::string stringWithOnlyNewlines(100, '\n');
    Lexer lexer4(stringWithOnlyNewlines);
    EXPECT_EQ(lexer4.NextToken().Type, TokenType::EndOfFile);

    const std::string stringWithCarriageReturn(100, '\r');
    Lexer lexer5(stringWithCarriageReturn);
    EXPECT_EQ(lexer5.NextToken().Type, TokenType::EndOfFile);

    std::string stringWithCRAndLF;
    for (int i = 0; i < 100; i++)
        stringWithCRAndLF += "\r\n";
    Lexer lexer6(stringWithCRAndLF);
    EXPECT_EQ(lexer6.NextToken().Type, TokenType::EndOfFile);

    std::string stringWithMixedNewlines;
    for (int i = 0; i < 100; i++)
    {
        if (i % 2 == 0)
            stringWithMixedNewlines += "\r\n";
        else if (i % 3 == 0)
            stringWithMixedNewlines += "\r";
        else
            stringWithMixedNewlines += "\n";
    }
    Lexer lexer7(stringWithMixedNewlines);
    EXPECT_EQ(lexer7.NextToken().Type, TokenType::EndOfFile);

    std::string veryLongEmptyString(1000000, ' ');
    Lexer lexer8(veryLongEmptyString);
    EXPECT_EQ(lexer8.NextToken().Type, TokenType::EndOfFile);
}

TEST_F(LexerTest, TestCommentParsing)
{
    const std::string src =
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

TEST_F(LexerTest, TestNumberParsing)
{
    const std::string src = R"(
        0123456789
        0b1010101010101010101010101010101010101010101010101010101010101010
        0b1010102010101010101010101010101010101010101010101010101010101010
        0x1234567890ABCDEFabcdef
        0x1234567890ABCDEFabcdefp1234567890
        0o12345670
        0o1234568
        5f
        5F
        5.0f
        5.0F
        5.0e5f
        5.0e5F
        2.0e-5f
        5e5f
        5e5F
        2e-5f
        5.0e+5f
        5.0e+5F
        2.0e-5f
        5.0e5
        5.0.0
        5.0e5.0
        5d
        5D
        5.0d
        5.0D
        5.0e5d
        5.0e5D
        2.0e-5d
        5e5d
        5e5D
        2e-5d
        5.0e+5d
        5.0e+5D
        2.0e-5d
        5.0e5d
        5.0.0
        5.0e5.0
        5l
        5L
        5ul
        5UL
        5u
        5U
        4ull
        4ULL
        3ll
        3LL
    )";

    // Technically mixing upper and lower cases are allowed, but we won't test that.

    Lexer lexer(src);
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberInt); // 0123456789
    EXPECT_EQ(lexer.NextToken().Type, TokenType::BinaryNumber); // 0b1010101010101010101010101010101010101010101010101010101010101010
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 0b1010102010101010101010101010101010101010101010101010101010101010
    (void)lexer.NextToken(); // The next number will be whatever is left after the '2' in the binary because it failed.
    EXPECT_EQ(lexer.NextToken().Type, TokenType::HexNumber); // 0x1234567890ABCDEFabcdef
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 0x1234567890ABCDEFabcdefp1234567890
    (void)lexer.NextToken(); // The next number will be whatever is left after the 'p' in the hex because it failed.
    EXPECT_EQ(lexer.NextToken().Type, TokenType::OctalNumber); // 0o12345670
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 0o12345678
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5F
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0F
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0e5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0e5F
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 2.0e-5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5e5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5e5F
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 2e-5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0e+5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0e+5F
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 2.0e-5f
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberFloat); // 5.0e5
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 5.0.0
    (void)lexer.NextToken(); // The next token will be the '0' after the second dot because it failed and consumed the dot.
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 5.0e5.0
    (void)lexer.NextToken();
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5D
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0D
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0e5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0e5D
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 2.0e-5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5e5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5e5D
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 2e-5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0e+5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0e+5D
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 2.0e-5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberDouble); // 5.0e5d
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 5.0.0
    (void)lexer.NextToken(); // The next token will be the '0' after the second dot because it failed and consumed the dot.
    EXPECT_EQ(lexer.NextToken().Type, TokenType::InvalidNumber); // 5.0e5.0
    (void)lexer.NextToken();
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberLong); // 5l
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberLong); // 5L
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsignedLong); // 5ul
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsignedLong); // 5UL
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsigned); // 5u
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsigned); // 5U
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsignedLongLong); // 4ull
    EXPECT_EQ(lexer.NextToken().Type, TokenType::NumberUnsignedLongLong); // 4ULL

}
