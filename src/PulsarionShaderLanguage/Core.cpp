#include "Core.hpp"

namespace Pulsarion::Shader
{
    std::string SeverityToString(ErrorSeverity severity)
    {
        switch (severity)
        {
        case ErrorSeverity::Suggestion:
            return "Suggestion";
        case ErrorSeverity::Warning:
            return "Warning";
        case ErrorSeverity::Fatal:
            return "Fatal";
        default:
            return "Unknown";
        }
    }
}
