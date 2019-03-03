#pragma once
#include <drogon/HttpSimpleController.h>
using namespace drogon;
class DbCtrlRaw:public drogon::HttpSimpleController<DbCtrlRaw>
{
public:
    virtual void asyncHandleHttpRequest(const HttpRequestPtr& req,const std::function<void (const HttpResponsePtr &)> & callback) override;
    PATH_LIST_BEGIN
    //list path definitions here;
    //PATH_ADD("/db", Get);
    PATH_LIST_END
};
