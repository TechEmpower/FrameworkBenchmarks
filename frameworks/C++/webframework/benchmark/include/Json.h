#pragma once

#include <Executors/StatelessExecutor.hpp>

namespace executor
{
	class Json : public framework::StatelessExecutor
	{
	public:
		void doGet(framework::HttpRequest& request, framework::HttpResponse& response) override;
	};
}
