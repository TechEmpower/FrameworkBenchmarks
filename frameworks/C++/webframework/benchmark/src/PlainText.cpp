#include "PlainText.h"

namespace executor
{
	void PlainText::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		response.addHeader("Content-Type", "text/plain");
		response.addHeader("Connection", "Keep-Alive");
		response.setBody("Hello, World!");
	}

	DEFINE_EXECUTOR(PlainText);
}
