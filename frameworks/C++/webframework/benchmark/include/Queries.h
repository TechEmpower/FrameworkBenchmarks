#pragma once

#include <Executors/HeavyOperationStatelessExecutor.hpp>

#include <random>

namespace executor
{
	class Queries : public framework::HeavyOperationStatelessExecutor
	{
	private:
		std::mt19937_64 random;

	public:
		static int normalizeQueries(framework::HttpRequest& request);

	public:
		void doGet(framework::HttpRequest& request, framework::HttpResponse& response) override;
	};
}
