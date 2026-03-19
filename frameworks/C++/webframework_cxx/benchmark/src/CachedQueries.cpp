#include "CachedQueries.h"

#include <Utility/WebFrameworkUtility.hpp>

#include "Database.h"
#include "Queries.h"

namespace executor
{
	void CachedQueries::init(const framework::utility::ExecutorSettings& settings)
	{
		framework::Database database = settings.getOrCreateDatabase("Benchmark");
		framework::Table table = database.getTable("CachedWorld");
		framework::Table connect = settings.getOrCreateDatabase<framework::RedisDatabase>("127.0.0.1:10010:password").getOrCreateTable("", "");

		framework::SqlResult result = table.execute("SELECT * FROM CachedWorld");

		for (const framework::SqlResult::Row& row : result)
		{
			connect.execute
			(
				"HSET",
				framework::utility::database::makeSQLValues(std::format("cache:{}", row.at("id").get<int64_t>()), "randomNumber", row.at("randomNumber").get<int64_t>())
			);
		}
	}

	void CachedQueries::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		framework::Table connect = request.getTable<framework::RedisDatabase>("127.0.0.1:10010:password", "");
		int queries = Queries::normalizeQueries(request);
		framework::JsonObject result;
		std::ostringstream stream;

		for (int i = 0; i < queries; i++)
		{
			int id = random() % Database::rows;
			int64_t randomNumber = 0;
			framework::JsonObject object;

			framework::SqlResult sqlResult = connect.execute
			(
				"HGET",
				framework::utility::database::makeSQLValues(std::format("cache:{}", id), "randomNumber")
			);

			randomNumber = sqlResult[0]["randomNumber"].get<int64_t>();

			object["id"] = sqlResult[0]["id"].get<int64_t>();
			object["randomNumber"] = randomNumber;

			result.emplace_back(std::move(object));
		}

		stream << result;

		response.setBody(stream.str());
	}

	DEFINE_EXECUTOR(CachedQueries)
}
