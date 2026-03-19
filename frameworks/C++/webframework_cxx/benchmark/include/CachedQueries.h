#pragma once

#include <Executors/StatelessExecutor.hpp>

#include <random>

namespace executor
{
	class CachedQueries : public framework::StatelessExecutor
	{
	private:
		std::mt19937_64 random;

	public:
		void init(const framework::utility::ExecutorSettings& settings) override;

		void doGet(framework::HttpRequest& request, framework::HttpResponse& response) override;
	};
}
