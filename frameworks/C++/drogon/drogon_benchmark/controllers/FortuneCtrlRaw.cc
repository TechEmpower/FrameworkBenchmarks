#include "FortuneCtrlRaw.h"
#include "models/Fortune.h"
#include <algorithm>

using namespace drogon_model::hello_world;
void FortuneCtrlRaw::asyncHandleHttpRequest(const HttpRequestPtr &req, const std::function<void(const HttpResponsePtr &)> &callback)
{
    auto client = drogon::app().getFastDbClient();
    *client << "select * from fortune where 1=$1"
            << 1 >>
        [callback](const Result &r) {
            std::vector<std::pair<int, std::string>> rows;
            for (auto row : r)
            {
                rows.emplace_back(row["id"].as<int>(), row["message"].as<std::string>());
            }
            rows.emplace_back(0, "Additional fortune added at request time.");
            std::sort(rows.begin(), rows.end(), [](const std::pair<int, std::string> &p1, const std::pair<int, std::string> &p2) -> bool {
                if (p1.second < p2.second)
                    return true;
                return false;
            });
            HttpViewData data;
            data.insert("rows", std::move(rows));
            auto resp = HttpResponse::newHttpViewResponse("fortune_raw.csp", data);
            callback(resp);
        } >>
        [callback](const DrogonDbException &err) {
            auto resp = HttpResponse::newHttpResponse();
            resp->setBody(std::string("error:") + err.base().what());
            callback(resp);
        };
}