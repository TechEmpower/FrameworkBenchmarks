#pragma once
#include <drogon/HttpSimpleController.h>
using namespace drogon;
class PlaintextCtrl : public drogon::HttpSimpleController<PlaintextCtrl>
{
  public:
    virtual void asyncHandleHttpRequest(
        const HttpRequestPtr &req,
        std::function<void(const HttpResponsePtr &)> &&callback) override;
    PATH_LIST_BEGIN
    // list path definitions here;
    // PATH_ADD("/plaintext",Get);
    PATH_LIST_END
};
