#include "Updates.h"

#include <Utility/WebFrameworkUtility.hpp>

#include "Database.h"
#include "Queries.h"

namespace executor
{
	void Updates::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		framework::Table table = request.getTable("Benchmark", "World");
		int queries = Queries::normalizeQueries(request);
		framework::JsonObject result;
		std::ostringstream stream;

		for (int i = 0; i < queries; i++)
		{
			int id = random() % Database::rows;
			size_t newValue = random();
			framework::JsonObject object;

			framework::SqlResult sqlResult = table.execute
			(
				"SELECT id, randomNumber FROM World WHERE id = ?",
				framework::utility::database::makeSQLValues(id)
			);

			table.execute
			(
				"UPDATE World SET randomNumber = ? WHERE id = ?",
				framework::utility::database::makeSQLValues(newValue, id)
			);

			object["id"] = sqlResult[0]["id"].get<int64_t>();
			object["randomNumber"] = newValue;

			result.emplace_back(std::move(object));
		}

		stream << result;

		response.setBody(stream.str());
	}

	DEFINE_EXECUTOR(Updates)
}
