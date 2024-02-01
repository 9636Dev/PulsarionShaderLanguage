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

TEST_F(LexerTest, TestInit)
{
    const std::string emptyStr = "";
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
    const std::string src =
        "0123456789\n"
        "0b1010101010101010101010101010101010101010101010101010101010101010\n" // Binary
        "0b1010102010101010101010101010101010101010101010101010101010101010\n" // Invalid binary
        "0x1234567890ABCDEFabcdef\n" // Hex
        "0x1234567890ABCDEFabcdefp1234567890\n" // Invalid hex 'p' character
        "0o12345670\n" // Octal
        "0o12345678\n" // Invalid octal"
        "5f\n" // Float
        "5F\n"
        "5.0f\n"
        "5.0F\n"
        "5.0e5f\n"
        "5.0e5F\n"
        "2.0e-5f\n"
        "5e5f\n"
        "5e5F\n"
        "2e-5f\n"
        "5.0e+5f\n"
        "5.0e+5F\n"
        "2.0e-5f\n"
        "5.0e5\n"
        "5.0.0\n" // Invalid float
        "5.0e5.0\n" // Invalid float
        "5d\n" // Double
        "5D\n"
        "5.0d\n"
        "5.0D\n"
        "5.0e5d\n"
        "5.0e5D\n"
        "2.0e-5d\n"
        "5e5d\n"
        "5e5D\n"
        "2e-5d\n"
        "5.0e+5d\n"
        "5.0e+5D\n"
        "2.0e-5d\n"
        "5.0e5d\n"
        "5.0.0\n" // Invalid double
        "5.0e5.0\n" // Invalid double
        "5l\n" // Long
        "5L\n"
        "5ul\n" // Unsigned long
        "5UL\n"
        "5u" // Unsigned
        "5U\n"
        "4ull\n" // Unsigned long long
        "4ULL\n"
        "3ll"
        "3LL\n";

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
