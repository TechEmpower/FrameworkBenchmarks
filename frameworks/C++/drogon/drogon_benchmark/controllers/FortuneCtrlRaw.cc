#include "FortuneCtrlRaw.h"
#include <drogon/orm/Result.h>
#include <algorithm>

using namespace drogon::orm;

void FortuneCtrlRaw::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));

    **_dbClient << "select * from fortune where 1=$1" << 1 >>
        [callbackPtr](const Result &r) {
            std::vector<Fortune> rows;
            rows.reserve(r.size() + 1);
            for (auto const &row : r)
            {
                rows.emplace_back(row[0ul].as<string_view>(),   // id
                                  row[1ul].as<string_view>());  // message
            }
            Fortune newRow;
            newRow.id_ = "0";
            newRow.message_ = "Additional fortune added at request time.";
            rows.emplace_back(std::move(newRow));
            std::sort(rows.begin(),
                      rows.end(),
                      [](const Fortune &f1, const Fortune &f2) -> bool {
                          if (f1.message_ < f2.message_)
                              return true;
                          else
                          {
                              return false;
                          }
                      });
            HttpViewData data;
            data.insert("rows", std::move(rows));
            auto resp =
                HttpResponse::newHttpViewResponse("fortune_raw.csp", data);
            (*callbackPtr)(resp);
        } >>
        [callbackPtr](const DrogonDbException &err) {
            auto resp = HttpResponse::newHttpResponse();
            resp->setBody(std::string("error:") + err.base().what());
            (*callbackPtr)(resp);
        };
}
