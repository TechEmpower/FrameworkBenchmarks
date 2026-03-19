#include "Queries.h"

#include <sstream>

#include <Utility/WebFrameworkUtility.hpp>

#include "Database.h"

namespace executor
{
	int Queries::normalizeQueries(framework::HttpRequest& request)
	{
		std::string_view queries = request.getQueryParameters().at("queries");

		if (std::all_of(queries.begin(), queries.end(), [](char c) { return std::isalnum(static_cast<uint32_t>(c)); }))
		{
			int result = std::stol(queries.data());

			return std::clamp(result, 1, 500);
		}

		return 1;
	}

	void Queries::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		framework::Table table = request.getTable("Benchmark", "World");
		int queries = Queries::normalizeQueries(request);
		framework::JsonObject result;
		std::ostringstream stream;

		for (int i = 0; i < queries; i++)
		{
			int id = random() % Database::rows;
			int64_t randomNumber = 0;
			framework::JsonObject object;

			framework::SqlResult sqlResult = table.execute
			(
				"SELECT id, randomNumber FROM World WHERE id = ?",
				framework::utility::database::makeSQLValues(id)
			);

			randomNumber = sqlResult[0]["randomNumber"].get<int64_t>();

			object["id"] = sqlResult[0]["id"].get<int64_t>();
			object["randomNumber"] = randomNumber;

			result.emplace_back(std::move(object));
		}

		stream << result;

		response.setBody(stream.str());
	}

	DEFINE_EXECUTOR(Queries);
}
