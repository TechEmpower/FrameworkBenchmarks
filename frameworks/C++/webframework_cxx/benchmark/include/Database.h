#pragma once

#include <Executors/HeavyOperationStatelessExecutor.hpp>

#include <random>

namespace executor
{
	class Database : public framework::HeavyOperationStatelessExecutor
	{
	public:
		static constexpr size_t rows = 10'000;

	private:
		std::mt19937_64 random;

	public:
		void init(const framework::utility::ExecutorSettings& settings) override;

		void doGet(framework::HttpRequest& request, framework::HttpResponse& response) override;
	};
}
