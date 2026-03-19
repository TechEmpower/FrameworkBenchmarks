#include "Database.h"

#include <Utility/WebFrameworkUtility.hpp>

namespace executor
{
	void Database::init(const framework::utility::ExecutorSettings& settings)
	{
		framework::Database database = settings.getOrCreateDatabase("Benchmark");
		framework::Table table = database.getOrCreateTable("World", "CREATE TABLE IF NOT EXISTS World (id INT PRIMARY KEY, randomNumber INT NOT NULL)");

		framework::SqlResult result = table.execute("SELECT COUNT(*) as count FROM World");

		if (result[0]["count"].get<int64_t>() != Database::rows)
		{
			for (size_t i = 0; i < Database::rows; i++)
			{
				table.execute
				(
					"INSERT INTO World (id, randomNumber) VALUES(?, ?)",
					framework::utility::database::makeSQLValues(i, random() % std::numeric_limits<int>::max())
				);
			}
		}
	}

	void Database::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		framework::Table table = request.getTable("Benchmark", "World");
		int id = random() % Database::rows;
		framework::JsonBuilder result;

		framework::SqlResult sqlResult = table.execute
		(
			"SELECT id, randomNumber FROM WORLD WHERE id = ?",
			framework::utility::database::makeSQLValues(id)
		);

		result["id"] = sqlResult[0]["id"].get<int64_t>();
		result["randomNumber"] = sqlResult[0]["randomNumber"].get<int64_t>();

		response.setBody(result);
	}

	DEFINE_EXECUTOR(Database);
}
