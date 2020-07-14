#include "UpdatesCtrl.h"
#include "models/World.h"
#include <stdlib.h>

using namespace drogon_model::hello_world;

void UpdatesCtrl::asyncHandleHttpRequest(
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
    auto json = std::make_shared<Json::Value>();
    json->resize(0);
    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    auto errFlag = std::make_shared<bool>(false);
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }
    drogon::orm::Mapper<World> mapper(*_dbClient);

    for (int i = 0; i < queries; i++)
    {
        World::PrimaryKeyType id = rand() % 10000 + 1;
        mapper.findByPrimaryKey(
            id,
            [callbackPtr, errFlag, json, &client = *_dbClient, queries](
                World w) mutable {
                if (*errFlag)
                    return;
                w.setRandomnumber(rand() % 10000 + 1);
                drogon::orm::Mapper<World> mapper(client);
                mapper.update(
                    w,
                    [w, json = std::move(json), errFlag, callbackPtr, queries](
                        const size_t count) {
                        if (*errFlag)
                            return;
                        json->append(w.toJson());
                        if (json->size() == queries)
                        {
                            (*callbackPtr)(HttpResponse::newHttpJsonResponse(
                                std::move(*json)));
                        }
                    },
                    [callbackPtr, errFlag](const DrogonDbException &e) {
                        if (*errFlag)
                            return;
                        *errFlag = true;
                        Json::Value json{};
                        json["code"] = 1;
                        json["message"] = e.base().what();
                        (*callbackPtr)(
                            HttpResponse::newHttpJsonResponse(std::move(json)));
                    });
            },
            [callbackPtr, errFlag](const DrogonDbException &e) {
                if (*errFlag)
                    return;
                *errFlag = true;
                Json::Value json{};
                json["code"] = 1;
                json["message"] = e.base().what();
                (*callbackPtr)(
                    HttpResponse::newHttpJsonResponse(std::move(json)));
            });
    }
}