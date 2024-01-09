#pragma once

#include <PulsarionCore/Core.hpp>

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
}
