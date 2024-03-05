#include "TypeCheck.hpp"
#include "PulsarionShaderLanguage/ParserUtil.hpp"

namespace Pulsarion::Shader
{
    /*
    *   We assume that the data is correct and only assert highly fatal errors like out-of-bounds access.
    *   This is because the data is generated by the parser and lexer, which are assumed to be correct, and user input should never be used here.
    *   User input should be validated in the parser and lexer or by a custom validator if it is not using the front-end.
    */
    void TypeChecker::GenerateTypeInformation()
    {
        using namespace Parsing;
        AbstractSyntaxTree ast(m_AST);
        std::size_t scopeIndex = 0; // So we don't repeat scope names for unnamed scopes
        bool ignoreScope = false; // We don't want an unnamed scope to be added to functions and structs
        std::vector<std::string> currentNamespace;

        // We try to parse without taking into account of forward declarations, this way we reduce the complexity of the language
        // If that doesn't work then we have to do the C++ style where we parse the while file in-order

        ast.Traverse([&, this](SyntaxNode& node, TraversalPhase phase)
        {
            switch (node.Type)
            {
            case NodeType::VariableDeclaration: {
                if (phase != TraversalPhase::Advance)
                    break;

                PULSARION_ASSERT(node.Children.size() == 2, "Variable declaration must have 2 children");
                auto primType = ParseVariablePrimType(node.Children[0]);
                auto name = ParseIdentifierFromNode(node.Children[1], currentNamespace);
                auto type = ParseIdentifierFromNode(node.Children[0], {});
                if (!type.has_value() || !name.has_value())
                {
                    PULSARION_LOG_ERROR("Failed to parse variable declaration"); // There should be no errors here but just a safety check
                    return false;
                }

                m_TypeInfo.Variables[name.value()] = VariableType{ primType, type.value(), std::nullopt };
                return true;
            }
            case NodeType::StructDefinition: {
                if (phase == TraversalPhase::Return)
                {
                    currentNamespace.pop_back();
                    break;
                }

                PULSARION_ASSERT(node.Children.size() == 2, "Struct declaration must have a children"); // Name and scope
                currentNamespace.emplace_back(node.Children[0].Content->Value);
                ignoreScope = true;
                break;
            }
            case NodeType::FunctionDefinition: {
                if (phase == TraversalPhase::Return)
                {
                    currentNamespace.pop_back();
                    break;
                }

                PULSARION_ASSERT(node.Children.size() == 4, "Function definition must have 3 children"); // Return type, name and scope
                currentNamespace.emplace_back(node.Children[1].Content->Value);
                ignoreScope = true;
                break;
            }
            case NodeType::Scope: {
                if (ignoreScope)
                {
                    ignoreScope = false;
                    break;
                }

                if (phase == TraversalPhase::Return)
                {
                    currentNamespace.pop_back();
                    break;
                }

                //std::string scopeName = std::string("UnnamedScope_") + std::to_string(scopeIndex++);
                //currentNamespace.push_back(std::move(scopeName));
                currentNamespace.emplace_back("");
                break;
            }
            default:
                break;
            }

            return false;
        });

    };
}
