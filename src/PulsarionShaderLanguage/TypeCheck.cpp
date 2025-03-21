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
        AbstractSyntaxTree ast(m_AST);
    };
}
