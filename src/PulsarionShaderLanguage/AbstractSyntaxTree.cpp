#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{
    SyntaxNode::SyntaxNode(NodeDescriptor descriptor, std::vector<SyntaxNode> children)
        : m_descriptor(descriptor)
        , m_children(children)
    {
    }

    const NodeDescriptor& SyntaxNode::GetDescriptor() const
    {
        return m_descriptor;
    }

    std::vector<SyntaxNode>& SyntaxNode::GetChildren()
    {
        return m_children;
    }
}
