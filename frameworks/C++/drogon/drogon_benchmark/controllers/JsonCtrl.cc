#include "JsonCtrl.h"
void JsonCtrl::asyncHandleHttpRequest(
    const HttpRequestPtr &req,
    std::function<void(const HttpResponsePtr &)> &&callback)
{
    Json::Value ret;
    ret["message"] = "Hello, World!";
    callback(HttpResponse::newHttpJsonResponse(std::move(ret)));
}