#pragma once

#include <PulsarionCore/Core.hpp>
#include <PulsarionCore/Assert.hpp>

#ifdef PULSARION_SHADER_LANGUAGE_BUILD_DLL
	#define PULSARION_SHADER_LANGUAGE_API PULSARION_DLL_EXPORT
#else
	#define PULSARION_SHADER_LANGUAGE_API PULSARION_DLL_IMPORT
#endif

namespace Pulsarion::Shader
{
    enum class ErrorSeverity
    {
        Suggestion,
        Warning,
        Fatal
    };

    /// <summary>
    /// Represents a location in the source code.
    /// </summary>
    struct SourceLocation
    {
        /// <summary>
        /// The line number.
        /// </summary>
        std::size_t Line;

        /// <summary>
        /// The column number.
        /// </summary>
        std::size_t Column;

        /// <summary>
        /// The character index.
        /// <summary>
        std::size_t Index;

        /// <summary>
        /// Length of the source location.
        /// </summary>
        std::size_t Length;

        SourceLocation(std::size_t line, std::size_t column, std::size_t index, std::size_t length)
            : Line(line), Column(column), Index(index), Length(length)
        {
        }
    };

}
