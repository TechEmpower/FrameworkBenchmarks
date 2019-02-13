#include "PlaintextCtrl.h"
void PlaintextCtrl::asyncHandleHttpRequest(const HttpRequestPtr& req,const std::function<void (const HttpResponsePtr &)> & callback)
{
    auto resp = HttpResponse::newHttpResponse();
    resp->setBody("Hello, World!");
    resp->setContentTypeCode(CT_TEXT_PLAIN);
//    resp->setExpiredTime(0);
    callback(resp);
}
