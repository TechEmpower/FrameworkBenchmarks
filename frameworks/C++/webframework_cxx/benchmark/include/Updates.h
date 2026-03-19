#pragma once

#include <Executors/HeavyOperationStatelessExecutor.hpp>

#include <random>

namespace executor
{
	class Updates : public framework::HeavyOperationStatelessExecutor
	{
	private:
		std::mt19937_64 random;

	public:
		void doGet(framework::HttpRequest& request, framework::HttpResponse& response) override;
	};
}
