#include "AbstractSyntaxTree.hpp"

#include <sstream>

namespace Pulsarion::Shader
{
    std::string NodeTypeToString(NodeType type)
    {
        switch (type)
        {
        case NodeType::Scope:
            return "Scope";
        case NodeType::Token:
            return "Token";
        default:
            return "Unknown";
        }
    }

    std::string SyntaxNode::ToString() const
    {
        std::stringstream ss;
        ss << NodeTypeToString(Type);
        if (Content.has_value())
        {
            ss << " " << Content.value().ToString();
        }
        ss << " {";
        for (const auto& child : Children)
        {
            ss << "\t" << child.ToString() << "\n";
        }
        ss << " }";
        return ss.str();
    }
}
