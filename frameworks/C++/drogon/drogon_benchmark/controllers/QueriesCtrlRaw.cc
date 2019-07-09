#include "QueriesCtrlRaw.h"
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
        std::shared_ptr<std::function<void(const HttpResponsePtr &)>>(
            new std::function<void(const HttpResponsePtr &)>(
                std::move(callback)));
    auto counter = std::make_shared<int>(queries);
    auto client = app().getFastDbClient();
    auto jsonStr = std::make_shared<std::string>();
    jsonStr->reserve(queries * 36);
    jsonStr->append("[", 1);
    for (int i = 0; i < queries; i++)
    {
        int id = rand() % 10000 + 1;
        *client << "select randomnumber from world where id=$1" << id >>
            [callbackPtr, counter, jsonStr, id](const Result &r) mutable {
                (*counter)--;
                if (r.size() > 0)
                {
                    char json[64];
                    auto size = sprintf(json,
                                        "{\"id\":%d,\"randomnumber\":%s}",
                                        id,
                                        r[0]["randomnumber"].c_str());
                    jsonStr->append(json, size);
                }
                if ((*counter) == 0)
                {
                    jsonStr->append("]", 1);
                    auto resp = HttpResponse::newHttpResponse();
                    resp->setContentTypeCode(ContentType::CT_APPLICATION_JSON);
                    resp->setBody(std::move(*jsonStr));
                    (*callbackPtr)(resp);
                }
                else
                {
                    jsonStr->append(",");
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