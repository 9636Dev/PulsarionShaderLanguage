#pragma once

#include "Core.hpp"
#include "AbstractSyntaxTree.hpp"

namespace Pulsarion::Shader
{
    class BaseOptimizer
    {
    public:
        virtual ~BaseOptimizer() = default;

        virtual void Optimize(SyntaxNode& ast) = 0;
    };

    class NoOptimizer : public BaseOptimizer
    {
    public:
        NoOptimizer() = default;
        ~NoOptimizer() override = default;

        void Optimize([[maybe_unused]] SyntaxNode& ast) override
        {
            // Do nothing
        }
    };

    class PULSARION_SHADER_LANGUAGE_API DefaultOptimizer : public BaseOptimizer
    {
    public:
        DefaultOptimizer() = default;
        ~DefaultOptimizer() override = default;

        void Optimize(SyntaxNode& ast) override;
    };


}
