#include "FortuneCtrl.h"
#include "models/Fortune.h"
#include <algorithm>

using namespace drogon_model::hello_world;
void FortuneCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    if (!*dbClient_)
    {
        *dbClient_ = drogon::app().getFastDbClient();
    }
    drogon::orm::Mapper<Fortune> mapper(*dbClient_);
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));

    mapper.findAll(
        [callbackPtr, this](std::vector<Fortune> rows) {
            Fortune newRow;
            newRow.setId(0);
            newRow.setMessage("Additional fortune added at request time.");
            rows.emplace_back(std::move(newRow));
            std::sort(rows.begin(),
                      rows.end(),
                      [](const Fortune &f1, const Fortune &f2) -> bool {
                          if (f1.getValueOfMessage() < f2.getValueOfMessage())
                              return true;
                          else
                          {
                              return false;
                          }
                      });
            HttpViewData data;
            data.insert("rows", std::move(rows));
            auto resp = HttpResponse::newHttpResponse();
            resp->setBody(bodyTemplate_->genText(data));
            (*callbackPtr)(resp);
        },
        [callbackPtr](const DrogonDbException &err) {
            auto resp = HttpResponse::newHttpResponse();
            resp->setBody(std::string("error:") + err.base().what());
            (*callbackPtr)(resp);
        });
}
