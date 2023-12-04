#include "Core.hpp"

#include <string>
#include <vector>

namespace Pulsarion::Shader
{
	class GrammerNode
	{

	};

	class PULSARION_SHADER_LANGUAGE_API AndNode : public GrammerNode
	{
		std::vector<GrammerNode> Nodes;
	};

	class PULSARION_SHADER_LANGUAGE_API OrNode : public GrammerNode
	{
		std::vector<GrammerNode> Nodes;
	};

	class PULSARION_SHADER_LANGUAGE_API RepeatNode : public GrammerNode
	{
		std::size_t Min;
		std::size_t Max;
		GrammerNode Node;
	};

	struct GrammerConstruct
	{
		std::string Identifier;
		std::vector<GrammerNode> Nodes;
	};
}
