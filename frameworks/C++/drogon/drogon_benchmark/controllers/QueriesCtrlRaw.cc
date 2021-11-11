#include "QueriesCtrlRaw.h"
#include "World_raw.h"
#include <stdlib.h>

using namespace drogon::orm;
void QueriesCtrlRaw::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    // write your application logic here
    static std::once_flag once;
    std::call_once(once, []() { srand(time(NULL)); });
    int queries = 1;
    auto &parameter = req->getParameter("queries");
    if (!parameter.empty())
    {
        queries = atoi(parameter.c_str());
        if (queries > 500)
            queries = 500;
        else if (queries < 1)
            queries = 1;
    }
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    auto counter = std::make_shared<int>(queries);
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }
    auto json = std::make_shared<Json::Value>();
    json->resize(0);
    for (int i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        **_dbClient << "select * from world where id=$1" << id >>
            [callbackPtr, counter, json](const Result &r) mutable {
                if (*counter <= 0)
                    return;
                if (r.size() > 0)
                {
                    auto w = World(r[0]);
                    json->append(w.toJson());
                    (*counter)--;
                    if ((*counter) == 0)
                    {
                        (*callbackPtr)(HttpResponse::newHttpJsonResponse(
                            std::move(*json)));
                    }
                }
                else
                {
                    *counter = -1;
                    Json::Value json{};
                    json["code"] = 0;
                    json["message"] = "Internal error";
                    (*callbackPtr)(
                        HttpResponse::newHttpJsonResponse(std::move(json)));
                }
            } >>
            [callbackPtr, counter](const DrogonDbException &e) {
                if (*counter <= 0)
                    return;
                *counter = -1;
                Json::Value json{};
                json["code"] = 1;
                json["message"] = e.base().what();
                auto resp = HttpResponse::newHttpJsonResponse(std::move(json));
                (*callbackPtr)(resp);
            };
    }
}