#include "Fortunes.h"

static constexpr std::string_view htmlData = R"(<!DOCTYPE html>
<html>
<head><title>Fortunes</title></head>
<body>
<table>
<tr><th>id</th><th>message</th></tr>
{}
</table>
</body>
</html>)";

struct Fortune
{
	int64_t id;
	std::string message;

	inline bool operator < (const Fortune& other) const noexcept
	{
		return message < other.message;
	}
};

namespace executor
{
	void Fortunes::doGet(framework::HttpRequest& request, framework::HttpResponse& response)
	{
		constexpr std::string_view trOpen = "<tr>";
		constexpr std::string_view trClose = "</tr>";
		constexpr std::string_view tdOpen = "<td>";
		constexpr std::string_view tdClose = "</td>";
		constexpr std::string_view additionalMessage = "Additional fortune added at request time.";

		constexpr size_t trSize = trOpen.size() + trClose.size();
		constexpr size_t tdSize = tdOpen.size() * 2 + tdClose.size() * 2;
		constexpr size_t additionalRowSize = trSize + tdSize + 1; // \n

		framework::Table table = request.getTable("Benchmark", "Fortune");
		framework::SqlResult sqlResult = table.execute("SELECT id, message FROM Fortune");
		size_t resultSize = additionalMessage.size() + 1;
		std::vector<Fortune> data;
		std::string result;
		
		data.reserve(sqlResult.size() + 1);

		for (const framework::SqlResult::Row& row : sqlResult)
		{
			const std::string& message = row.at("message").get<std::string>();

			data.emplace_back(row.at("id").get<int64_t>(), message);

			resultSize += additionalRowSize + message.size() + 2; // id
		}

		data.emplace_back(data.rbegin()->id + 1, "Additional fortune added at request time.");

		std::sort(data.begin(), data.end());

		result.reserve(resultSize);

		for (const Fortune& fortune : data)
		{
			result += std::format("{}{}{}{}{}{}{}{}\n", trOpen, tdOpen, fortune.id, tdClose, tdOpen, fortune.message, tdClose, trClose);
		}

		response.setBody(std::format(htmlData, result));
	}

	DEFINE_EXECUTOR(Fortunes)
}
