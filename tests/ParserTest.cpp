#include <gtest/gtest.h>
#include <iostream>
#include <list>
#include "PulsarionShaderLanguage/Parser.hpp"

using namespace Pulsarion::Shader;

// We make a test fixture to avoid having to create a new Lexer for each test.
class ParserTest: public ::testing::Test
{
protected:
    ParserTest() = default;
};

void PrintErrors(const std::list<Parsing::Error>& errors)
{
    for (auto& error : errors)
    {
        std::cerr << "[" << error.Location.Line << ":" << error.Location.Column << "] " << Parsing::Error::SourceToString(error.Source) << " - " << Parsing::Error::TypeToString(error.Type) << std::endl;
    }
}

TEST_F(ParserTest, TestEmptyProgram)
{
    std::string program;
    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 0);
}

TEST_F(ParserTest, TestScopeParsing)
{
    std::string program = R"(
{
    float a;
    float b;
}
)";
    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto scope = ast.Root->Children[0];
    EXPECT_EQ(scope.Type, NodeType::Scope);
    EXPECT_EQ(scope.Children.size(), 2);
    EXPECT_EQ(scope.Children[0].Type, NodeType::VariableDeclaration);
    EXPECT_EQ(scope.Children[1].Type, NodeType::VariableDeclaration);
}

TEST_F(ParserTest, TestNestedScopeParsing)
{
    std::string program = R"(
{
    float a;
    {
        float b;
    }
}
)";
    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto scope = ast.Root->Children[0];
    EXPECT_EQ(scope.Type, NodeType::Scope);
    EXPECT_EQ(scope.Children.size(), 2);
    EXPECT_EQ(scope.Children[0].Type, NodeType::VariableDeclaration);
    EXPECT_EQ(scope.Children[1].Type, NodeType::Scope);
    auto nestedScope = scope.Children[1];
    EXPECT_EQ(nestedScope.Type, NodeType::Scope);
    EXPECT_EQ(nestedScope.Children.size(), 1);
    EXPECT_EQ(nestedScope.Children[0].Type, NodeType::VariableDeclaration);
}

TEST_F(ParserTest, TestFunctionDefinitionParsing)
{
    std::string program = R"(
float main()
{
    return 0.0;
}
)";
    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto function = ast.Root->Children[0];
    EXPECT_EQ(function.Type, NodeType::FunctionDefinition);
    EXPECT_EQ(function.Children.size(), 4); // Always ret type, name, arg list and scope
}

TEST_F(ParserTest, TestFunctionDefinitionWithParametersParsing)
{
    std::string program = R"(
float main(float a, float b)
{
    return 0.0;
}
)";
    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto function = ast.Root->Children[0];
    EXPECT_EQ(function.Type, NodeType::FunctionDefinition);
    EXPECT_EQ(function.Children.size(), 4);
    EXPECT_EQ(function.Children[2].Type, NodeType::FunctionArgumentList);
    auto parameterList = function.Children[2];
    EXPECT_EQ(parameterList.Children.size(), 2);
    EXPECT_EQ(parameterList.Children[0].Type, NodeType::FunctionArgument);
    EXPECT_EQ(parameterList.Children[1].Type, NodeType::FunctionArgument);
    EXPECT_EQ(function.Children[3].Type, NodeType::Scope);
}

TEST_F(ParserTest, TestFunctionDeclaration)
{
    std::string program = R"(
    void main();
)";

    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto function = ast.Root->Children[0];
    EXPECT_EQ(function.Type, NodeType::FunctionDeclaration);
}

/*
TEST_F(ParserTest, TestFunctionDeclarationWithParameters)
{
    std::string program = R"(
    void main(float a, float b);
)";

    Lexer lexer(program);
    Parser parser(std::move(lexer));
    auto ast = parser.Parse();
    EXPECT_TRUE(ast.Root.has_value());
    if (!ast.Root.has_value())
    {
        PrintErrors(ast.Errors);
        return;
    }

    EXPECT_EQ(ast.Root->Type, NodeType::Root);
    EXPECT_EQ(ast.Root->Children.size(), 1);
    auto function = ast.Root->Children[0];
    EXPECT_EQ(function.Type, NodeType::FunctionDeclaration);
    EXPECT_EQ(function.Children.size(), 3);
    EXPECT_EQ(function.Children[1].Type, NodeType::ArgumentList);
    auto parameterList = function.Children[1];
    EXPECT_EQ(parameterList.Children.size(), 2);
    EXPECT_EQ(parameterList.Children[0].Type, NodeType::FunctionArgument);
    EXPECT_EQ(parameterList.Children[1].Type, NodeType::FunctionArgument);
}
*/
