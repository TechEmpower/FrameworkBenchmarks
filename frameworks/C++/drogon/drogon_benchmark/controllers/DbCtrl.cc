#include "DbCtrl.h"
#include "models/World.h"
#include <drogon/utils/Utilities.h>
#include <thread>
#include <stdlib.h>
#include <time.h>
using namespace drogon_model::hello_world;

void DbCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    // write your application logic here
    static std::once_flag once;
    std::call_once(once, []() { srand(time(NULL)); });
    if (!*_dbClient)
    {
        *_dbClient = drogon::app().getFastDbClient();
    }

    auto callbackPtr =
        std::make_shared<std::function<void(const HttpResponsePtr &)>>(
            std::move(callback));
    drogon::orm::Mapper<World> mapper(*_dbClient);
    World::PrimaryKeyType id = rand() % 10000 + 1;
    mapper.findByPrimaryKey(
        id,
        [callbackPtr](World w) {
            auto j = w.toJson();
            auto resp = HttpResponse::newHttpJsonResponse(j);
            (*callbackPtr)(resp);
        },
        [callbackPtr](const DrogonDbException &e) {
            Json::Value ret;
            ret["result"] = "error!";
            auto resp = HttpResponse::newHttpJsonResponse(ret);
            (*callbackPtr)(resp);
        });
}
