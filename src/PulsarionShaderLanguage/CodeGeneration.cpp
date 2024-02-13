#include "CodeGeneration.hpp"

#include <stack>

namespace Pulsarion::Shader
{
    std::pair<BaseGenerator::SourceCode, std::unique_ptr<GeneratorMetadata>> CppGenerator::Generate(SyntaxNode& ast)
    {
        std::stack<SyntaxNode*> stack; // We don't need to free the memory of the nodes, so we can use raw pointers
        stack.push(&ast);
        return std::make_pair("", nullptr);
    }
}
