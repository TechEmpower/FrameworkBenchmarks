#include "QueriesCtrlRaw.h"
using namespace drogon::orm;
void QueriesCtrlRaw::asyncHandleHttpRequest(const HttpRequestPtr &req, const std::function<void(const HttpResponsePtr &)> &callback)
{
    //write your application logic here
    static std::once_flag once;
    std::call_once(once, []() {
        srand(time(NULL));
    });
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
    auto callbackPtr = std::shared_ptr<std::function<void(const HttpResponsePtr &)>>(new std::function<void(const HttpResponsePtr &)>(callback));
    auto counter = std::make_shared<int>(queries);
    auto client = app().getFastDbClient();
    for (int i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        *client << "select randomnumber from world where id=$1" << id >>
            [callbackPtr, counter, json, id](const Result &r) mutable {
                (*counter)--;
                if (r.size() > 0)
                {
                    Json::Value j;
                    j["id"] = id;
                    j["randomnumber"] = r[0]["randomnumber"].as<int>();
                    json->append(std::move(j));
                }
                if ((*counter) == 0)
                {
                    (*callbackPtr)(HttpResponse::newHttpJsonResponse(*json));
                }
            } >>
            [callbackPtr](const DrogonDbException &e) {
                Json::Value ret;
                ret["result"] = "error!";
                auto resp = HttpResponse::newHttpJsonResponse(ret);
                (*callbackPtr)(resp);
            };
    }
}