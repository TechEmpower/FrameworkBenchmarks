#include "Json.h"

namespace executor
{
	void Json::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		framework::JsonBuilder builder;

		builder["message"] = "Hello, World!";

		response.setBody(builder);
	}

	DEFINE_EXECUTOR(Json)
}
