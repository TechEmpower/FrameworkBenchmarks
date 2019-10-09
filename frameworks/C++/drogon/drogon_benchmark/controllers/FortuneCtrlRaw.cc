#include "FortuneCtrlRaw.h"
#include "models/Fortune.h"
#include <algorithm>

using namespace drogon_model::hello_world;
void FortuneCtrlRaw::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    auto client = drogon::app().getFastDbClient();
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));

    *client << "select * from fortune where 1=$1" << 1 >> [callbackPtr](
                                                              const Result &r) {
        std::vector<std::pair<string_view, string_view>> rows;
        for (auto const &row : r)
        {
            rows.emplace_back(row["id"].as<string_view>(),
                              row["message"].as<string_view>());
        }
        rows.emplace_back("0", "Additional fortune added at request time.");
        std::sort(rows.begin(),
                  rows.end(),
                  [](const std::pair<string_view, string_view> &p1,
                     const std::pair<string_view, string_view> &p2) -> bool {
                      if (p1.second < p2.second)
                          return true;
                      return false;
                  });
        HttpViewData data;
        data.insert("rows", std::move(rows));
        auto resp = HttpResponse::newHttpViewResponse("fortune_raw.csp", data);
        (*callbackPtr)(resp);
    } >> [callbackPtr](const DrogonDbException &err) {
        auto resp = HttpResponse::newHttpResponse();
        resp->setBody(std::string("error:") + err.base().what());
        (*callbackPtr)(resp);
    };
}
