#pragma once
#include <drogon/HttpSimpleController.h>
using namespace drogon;
class FortuneCtrl:public drogon::HttpSimpleController<FortuneCtrl>
{
public:
    virtual void asyncHandleHttpRequest(const HttpRequestPtr& req,const std::function<void (const HttpResponsePtr &)> & callback) override;
    PATH_LIST_BEGIN
    //list path definitions here;
    PATH_LIST_END
};
